import csv
import random
import math
import json
import pickle
import os
from datetime import datetime
from typing import Dict, List, Tuple, Any
from collections import defaultdict

class HardwarePredictor:
    """Hardware Prediction System - Optimized Version"""

    def __init__(self, save_to_current_dir: bool = True, clean_previous: bool = True):
        self.dataflow_mapping = {}
        self.feature_stats = {}
        self.save_dir = "." if save_to_current_dir else "model_artifacts"

        if not save_to_current_dir:
            os.makedirs(self.save_dir, exist_ok=True)

        if clean_previous and save_to_current_dir:
            self._cleanup_files()

        self.feature_columns = [
            'Dataflow_encoded', 'Total Number of Multipliers', 'R', 'C',
            'A', 'B', 'P', 'Streaming Dimension Size'
        ]

        self.configs = {
            'Area': {'sample_size': 90, 'alpha': 0.1, 'ensemble_size': 5, 'n_trials': 12},
            'Total Power': {'sample_size': 90, 'alpha': 0.1, 'ensemble_size': 5, 'n_trials': 12}
        }

    def _cleanup_files(self):
        """Clean up previous training files"""
        files_to_remove = [f for f in os.listdir('.') if 'hardware_predictor_' in f]
        for filename in files_to_remove:
            try:
                os.remove(filename)
            except:
                pass

    def load_csv(self, filepath: str) -> List[Dict]:
        """Load and parse CSV file"""
        data = []
        try:
            with open(filepath, 'r', newline='', encoding='utf-8-sig') as file:
                content = file.read()

            lines = [line.strip() for line in content.replace('\r\n', '\n').replace('\r', '\n').split('\n') if line.strip()]
            header = [col.strip() for col in lines[0].split(',')]

            for line in lines[1:]:
                values = [val.strip() for val in line.split(',')]
                if len(values) == len(header):
                    row = {}
                    for col, val in zip(header, values):
                        if col == 'Dataflow':
                            row[col] = val
                        else:
                            try:
                                row[col] = float(val) if '.' in val else int(val)
                            except:
                                row[col] = val
                    if 'Dataflow' in row and row['Dataflow']:
                        data.append(row)
        except Exception as e:
            print(f"Error loading {filepath}: {e}")
        return data

    def preprocess_data(self, train_path: str, val_path: str, test_path: str):
        """Load and preprocess all datasets"""
        train_data = self.load_csv(train_path)
        val_data = self.load_csv(val_path)
        test_data = self.load_csv(test_path)

        # Create dataflow mapping
        all_data = train_data + val_data + test_data
        unique_dataflows = list(set(row['Dataflow'] for row in all_data))
        self.dataflow_mapping = {df: i for i, df in enumerate(unique_dataflows)}

        # Encode dataflows
        for dataset in [train_data, val_data, test_data]:
            for row in dataset:
                row['Dataflow_encoded'] = self.dataflow_mapping[row['Dataflow']]

        return train_data, val_data, test_data

    def create_balanced_sample(self, train_data: List[Dict], sample_size: int) -> List[Dict]:
        """Create balanced sample from training data"""
        dataflow_groups = defaultdict(list)
        for row in train_data:
            dataflow_groups[row['Dataflow']].append(row)

        sampled_data = []
        num_dataflows = len(dataflow_groups)
        min_per_dataflow = max(3, sample_size // (num_dataflows * 2))

        for dataflow, rows in dataflow_groups.items():
            proportion = len(rows) / len(train_data)
            target_samples = max(min_per_dataflow, int(sample_size * proportion))
            actual_samples = min(target_samples, len(rows))
            sampled_data.extend(random.sample(rows, actual_samples))

        if len(sampled_data) < sample_size:
            remaining = [row for row in train_data if row not in sampled_data]
            additional = sample_size - len(sampled_data)
            if additional > 0 and remaining:
                sampled_data.extend(random.sample(remaining, min(additional, len(remaining))))

        return sampled_data[:sample_size]

    def normalize_features(self, train_data: List[Dict], val_data: List[Dict]) -> Tuple[List[Dict], List[Dict], Dict]:
        """Normalize features using min-max scaling"""
        feature_stats = {}

        for feature in self.feature_columns:
            values = [row[feature] for row in train_data if feature in row]
            if values:
                feature_stats[feature] = {
                    'min': min(values),
                    'max': max(values),
                    'range': max(values) - min(values)
                }

        def normalize_dataset(dataset):
            normalized = []
            for row in dataset:
                new_row = row.copy()
                for feature in self.feature_columns:
                    if feature in row and feature in feature_stats:
                        if feature_stats[feature]['range'] > 0:
                            new_row[f"{feature}_norm"] = (row[feature] - feature_stats[feature]['min']) / feature_stats[feature]['range']
                        else:
                            new_row[f"{feature}_norm"] = 0
                    else:
                        new_row[f"{feature}_norm"] = 0
                normalized.append(new_row)
            return normalized

        self.feature_stats = feature_stats
        return normalize_dataset(train_data), normalize_dataset(val_data), feature_stats

    def ridge_regression(self, train_data: List[Dict], test_point: Dict, target: str, alpha: float = 0.1) -> Tuple[float, List[float]]:
        """Ridge regression with coefficient return"""
        if len(train_data) < 2:
            avg_val = sum(row[target] for row in train_data) / len(train_data)
            return avg_val, [avg_val] + [0] * len(self.feature_columns)

        # Build matrices
        X, y = [], []
        for row in train_data:
            features = [1.0] + [row[f"{feature}_norm"] for feature in self.feature_columns]
            X.append(features)
            y.append(row[target])

        try:
            # Matrix operations for Ridge regression: (X^T X + αI)^(-1) X^T y
            def matrix_multiply(A, B):
                rows_A, cols_A = len(A), len(A[0])
                rows_B, cols_B = len(B), len(B[0])
                return [[sum(A[i][k] * B[k][j] for k in range(cols_A)) for j in range(cols_B)] for i in range(rows_A)]

            def matrix_transpose(matrix):
                return [[matrix[j][i] for j in range(len(matrix))] for i in range(len(matrix[0]))]

            def gaussian_elimination(matrix):
                n = len(matrix)
                for i in range(n):
                    # Find pivot
                    max_row = max(range(i, n), key=lambda r: abs(matrix[r][i]))
                    matrix[i], matrix[max_row] = matrix[max_row], matrix[i]

                    # Forward elimination
                    if abs(matrix[i][i]) > 1e-10:
                        for k in range(i + 1, len(matrix[0])):
                            matrix[i][k] /= matrix[i][i]
                        matrix[i][i] = 1.0

                        for k in range(i + 1, n):
                            if abs(matrix[k][i]) > 1e-10:
                                for j in range(i + 1, len(matrix[0])):
                                    matrix[k][j] -= matrix[k][i] * matrix[i][j]
                                matrix[k][i] = 0.0

                # Back substitution
                solution = [0.0] * n
                for i in range(n - 1, -1, -1):
                    solution[i] = matrix[i][n]
                    for j in range(i + 1, n):
                        solution[i] -= matrix[i][j] * solution[j]
                return solution

            X_T = matrix_transpose(X)
            XTX = matrix_multiply(X_T, X)

            # Add regularization
            for i in range(len(XTX)):
                XTX[i][i] += alpha

            XTy = [sum(X_T[i][j] * y[j] for j in range(len(y))) for i in range(len(X_T))]

            # Solve system
            n_params = len(self.feature_columns) + 1
            augmented = [XTX[i] + [XTy[i]] for i in range(n_params)]
            coefficients = gaussian_elimination(augmented)

            # Make prediction
            test_features = [1.0] + [test_point[f"{feature}_norm"] for feature in self.feature_columns]
            prediction = sum(coef * feat for coef, feat in zip(coefficients, test_features))
            return prediction, coefficients

        except:
            avg_val = sum(row[target] for row in train_data) / len(train_data)
            return avg_val, [avg_val] + [0] * len(self.feature_columns)

    def ensemble_predict(self, train_data: List[Dict], val_data: List[Dict], target: str, config: Dict) -> Tuple[List[float], List[List[float]]]:
        """Ensemble prediction with coefficient tracking"""
        all_predictions, all_coefficients = [], []

        for i in range(config['ensemble_size']):
            try:
                sample_data = self.create_balanced_sample(train_data, config['sample_size'])
                sample_norm, val_norm, _ = self.normalize_features(sample_data, val_data)

                predictions, model_coefficients = [], None
                for j, test_point in enumerate(val_norm):
                    pred, coeffs = self.ridge_regression(sample_norm, test_point, target, config['alpha'])
                    predictions.append(pred)
                    if j == 0:
                        model_coefficients = coeffs

                all_predictions.append(predictions)
                if model_coefficients:
                    all_coefficients.append(model_coefficients)
            except:
                pass

        if not all_predictions:
            return [0] * len(val_data), []

        # Ensemble averaging with outlier removal
        ensemble_predictions = []
        for i in range(len(all_predictions[0])):
            point_predictions = [pred[i] for pred in all_predictions]

            if len(point_predictions) >= 4:
                sorted_preds = sorted(point_predictions)
                n = len(sorted_preds)
                start_idx, end_idx = n // 5, n - n // 5
                trimmed_preds = sorted_preds[start_idx:end_idx]
            else:
                trimmed_preds = point_predictions

            ensemble_predictions.append(sum(trimmed_preds) / len(trimmed_preds))

        return ensemble_predictions, all_coefficients

    def calculate_metrics(self, y_true: List[float], y_pred: List[float]) -> Dict[str, float]:
        """Calculate prediction metrics"""
        n = len(y_true)
        mse = sum((true - pred) ** 2 for true, pred in zip(y_true, y_pred)) / n
        mae = sum(abs(true - pred) for true, pred in zip(y_true, y_pred)) / n
        mape = sum(abs((true - pred) / true) for true, pred in zip(y_true, y_pred) if true != 0) / n * 100

        y_mean = sum(y_true) / n
        ss_tot = sum((true - y_mean) ** 2 for true in y_true)
        ss_res = sum((true - pred) ** 2 for true, pred in zip(y_true, y_pred))
        r2 = 1 - (ss_res / ss_tot) if ss_tot > 0 else 0

        return {'MSE': mse, 'MAE': mae, 'RMSE': math.sqrt(mse), 'R2': r2, 'MAPE': mape}

    def multi_trial_predict(self, train_data: List[Dict], val_data: List[Dict], target: str, config: Dict) -> Dict[str, Any]:
        """Multi-trial prediction with model saving"""
        all_results, best_result, best_coefficients = [], None, None

        for trial in range(config['n_trials']):
            try:
                predictions, coefficients = self.ensemble_predict(train_data, val_data, target, config)
                true_values = [row[target] for row in val_data]
                metrics = self.calculate_metrics(true_values, predictions)

                trial_result = {
                    'trial': trial + 1,
                    'metrics': metrics,
                    'predictions': predictions,
                    'coefficients': coefficients
                }

                all_results.append(trial_result)

                if best_result is None or metrics['MAPE'] < best_result['metrics']['MAPE']:
                    best_result = trial_result
                    best_coefficients = coefficients

                # Early stopping
                if metrics['MAPE'] < 10.0 and metrics['R2'] > 0.95:
                    break
            except:
                pass

        if not all_results:
            return {'target': target, 'error': 'All trials failed'}

        # Save best model
        if best_result and best_coefficients:
            self._save_model(target, best_coefficients, config, best_result['metrics'])

        # Calculate summary metrics
        r2_scores = [r['metrics']['R2'] for r in all_results]
        mape_scores = [r['metrics']['MAPE'] for r in all_results]

        enhanced_metrics = {
            'R2': sum(r2_scores) / len(r2_scores),
            'MAPE': sum(mape_scores) / len(mape_scores),
            'min_MAPE': min(mape_scores),
            'best_R2': max(r2_scores),
            'trials_completed': len(all_results)
        }

        return {
            'target': target,
            'enhanced_metrics': enhanced_metrics,
            'best_result': best_result,
            'best_coefficients': best_coefficients
        }

    def _save_model(self, target: str, coefficients: List[List[float]], config: Dict, metrics: Dict):
        """Save model artifacts including JSON files"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        target_clean = target.replace(' ', '_').lower()

        # Save coefficients as JSON (human readable)
        coefficients_file = f"hardware_predictor_{target_clean}_coefficients.json"
        coefficients_data = {
            'timestamp': timestamp,
            'target': target,
            'feature_columns': self.feature_columns,
            'coefficients': coefficients,
            'ensemble_size': len(coefficients),
            'config': config,
            'metrics': metrics
        }
        with open(coefficients_file, 'w') as f:
            json.dump(coefficients_data, f, indent=2)

        # Save complete model as pickle
        model_file = f"hardware_predictor_{target_clean}_model.pkl"
        model_data = {
            'timestamp': timestamp,
            'target': target,
            'coefficients': coefficients,
            'feature_stats': self.feature_stats,
            'feature_columns': self.feature_columns,
            'dataflow_mapping': self.dataflow_mapping,
            'config': config,
            'metrics': metrics
        }
        with open(model_file, 'wb') as f:
            pickle.dump(model_data, f)

        # Save feature statistics as JSON
        stats_file = f"hardware_predictor_{target_clean}_feature_stats.json"
        with open(stats_file, 'w') as f:
            json.dump(self.feature_stats, f, indent=2)

    def load_model(self, model_file: str) -> Dict:
        """Load saved model"""
        try:
            with open(model_file, 'rb') as f:
                return pickle.load(f)
        except:
            return None

    def predict_both_targets(self, train_data: List[Dict], val_data: List[Dict]) -> Dict[str, Any]:
        """Train and predict both targets"""
        print("🔄 HARDWARE PREDICTION TRAINING")
        print("=" * 50)

        results = {}
        for target in ['Area', 'Total Power']:
            print(f"\n🎯 Training {target}...")
            result = self.multi_trial_predict(train_data, val_data, target, self.configs[target])
            results[target] = result

            if 'enhanced_metrics' in result:
                metrics = result['enhanced_metrics']
                print(f"   Best: R² = {metrics['best_R2']:.3f}, MAPE = {metrics['min_MAPE']:.2f}%")

        # Save predictions and summary
        self._save_predictions(val_data, results)
        self._save_training_summary(results)
        return results

    def _save_predictions(self, val_data: List[Dict], results: Dict[str, Any]):
        """Save validation predictions to CSV"""
        with open("hardware_predictor_predictions.csv", 'w', newline='') as csvfile:
            writer = csv.writer(csvfile)
            headers = ['Index', 'Dataflow', 'True_Area', 'Predicted_Area', 'True_Power', 'Predicted_Power']
            writer.writerow(headers)

            for i, val_row in enumerate(val_data):
                row_data = [
                    i + 1,
                    val_row['Dataflow'],
                    val_row['Area'],
                    results['Area']['best_result']['predictions'][i] if 'best_result' in results['Area'] else 0,
                    val_row['Total Power'],
                    results['Total Power']['best_result']['predictions'][i] if 'best_result' in results['Total Power'] else 0
                ]
                writer.writerow(row_data)

    def _save_training_summary(self, results: Dict[str, Any]):
        """Save overall training summary JSON"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        summary_file = f"hardware_predictor_training_summary.json"

        summary_data = {
            'timestamp': datetime.now().isoformat(),
            'training_session': timestamp,
            'dataflow_mapping': self.dataflow_mapping,
            'feature_columns': self.feature_columns,
            'configs': self.configs,
            'results_summary': {
                target: {
                    'best_metrics': results[target]['best_result']['metrics'] if 'best_result' in results[target] else None,
                    'enhanced_metrics': results[target].get('enhanced_metrics', {}),
                    'trials_completed': results[target].get('enhanced_metrics', {}).get('trials_completed', 0)
                } for target in ['Area', 'Total Power'] if target in results
            }
        }

        with open(summary_file, 'w') as f:
            json.dump(summary_data, f, indent=2)

    def predict_with_saved_model(self, model_file: str, new_samples: List[Dict]) -> List[float]:
        """Make predictions using saved model"""
        model_data = self.load_model(model_file)
        if not model_data:
            return []

        # Restore model state
        coefficients_list = model_data['coefficients']
        feature_stats = model_data['feature_stats']
        dataflow_mapping = model_data['dataflow_mapping']

        # Preprocess samples
        for sample in new_samples:
            sample['Dataflow_encoded'] = dataflow_mapping.get(sample['Dataflow'], 0)

            for feature in self.feature_columns:
                if feature in sample and feature in feature_stats:
                    if feature_stats[feature]['range'] > 0:
                        sample[f"{feature}_norm"] = (sample[feature] - feature_stats[feature]['min']) / feature_stats[feature]['range']
                    else:
                        sample[f"{feature}_norm"] = 0
                else:
                    sample[f"{feature}_norm"] = 0

        # Ensemble prediction
        all_predictions = []
        for coefficients in coefficients_list:
            sample_predictions = []
            for sample in new_samples:
                test_features = [1.0] + [sample[f"{feature}_norm"] for feature in self.feature_columns]
                prediction = sum(coef * feat for coef, feat in zip(coefficients, test_features))
                sample_predictions.append(prediction)
            all_predictions.append(sample_predictions)

        # Average predictions
        return [sum(pred[i] for pred in all_predictions) / len(all_predictions) for i in range(len(new_samples))]

    def predict_new_samples(self, train_data: List[Dict], new_samples: List[Dict]) -> Dict[str, List[float]]:
        """Predict new samples using training data"""
        print("\n🔮 NEW SAMPLE PREDICTIONS")
        print("-" * 30)

        predictions = {}
        for target in ['Area', 'Total Power']:
            # Encode new samples
            for sample in new_samples:
                sample['Dataflow_encoded'] = self.dataflow_mapping.get(sample['Dataflow'], 0)

            target_predictions, _ = self.ensemble_predict(train_data, new_samples, target, self.configs[target])
            predictions[target] = target_predictions

        # Save results with JSON metadata
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")

        with open("hardware_predictor_new_predictions.csv", 'w', newline='') as csvfile:
            writer = csv.writer(csvfile)
            headers = ['Sample_ID', 'Dataflow', 'Total_Multipliers', 'R', 'C', 'A', 'B', 'P',
                       'Streaming_Size', 'Predicted_Area', 'Predicted_Power']
            writer.writerow(headers)

            for i, sample in enumerate(new_samples):
                writer.writerow([
                    i + 1, sample['Dataflow'], sample['Total Number of Multipliers'],
                    sample['R'], sample['C'], sample['A'], sample['B'], sample['P'],
                    sample['Streaming Dimension Size'],
                    predictions['Area'][i], predictions['Total Power'][i]
                ])

        # Save prediction metadata as JSON
        prediction_metadata = {
            'timestamp': datetime.now().isoformat(),
            'prediction_session': timestamp,
            'input_samples': new_samples,
            'predictions': predictions,
            'dataflow_mapping': self.dataflow_mapping,
            'feature_columns': self.feature_columns
        }

        with open(f"hardware_predictor_new_predictions_metadata.json", 'w') as f:
            json.dump(prediction_metadata, f, indent=2)

        return predictions


def train_and_inference_mode():
    """Complete training and inference mode"""
    print("🚀 TRAINING + INFERENCE MODE")
    print("=" * 40)

    predictor = HardwarePredictor()
    random.seed(42)

    # Load and preprocess data
    train_data, val_data, test_data = predictor.preprocess_data('train.csv', 'validation.csv', 'test.csv')
    print(f"Loaded: {len(train_data)} train, {len(val_data)} val, {len(test_data)} test samples")

    # Train and validate
    results = predictor.predict_both_targets(train_data, val_data)

    # Test new samples
    test_samples = [
        {'Dataflow': 'OS', 'Total Number of Multipliers': 256, 'R': 16, 'C': 16, 'A': 1, 'B': 1, 'P': 1, 'Streaming Dimension Size': 1},
        {'Dataflow': 'WS', 'Total Number of Multipliers': 512, 'R': 8, 'C': 8, 'A': 2, 'B': 2, 'P': 2, 'Streaming Dimension Size': 64},
        {'Dataflow': 'IS', 'Total Number of Multipliers': 1024, 'R': 32, 'C': 32, 'A': 1, 'B': 1, 'P': 1, 'Streaming Dimension Size': 128}
    ]

    predictions = predictor.predict_new_samples(train_data, test_samples)

    print(f"\n📊 NEW SAMPLE RESULTS:")
    for i, sample in enumerate(test_samples):
        print(f"Sample {i+1} ({sample['Dataflow']}): Area={predictions['Area'][i]:.1f}, Power={predictions['Total Power'][i]:.1f}")

    print(f"\n✅ Training completed - models saved to current directory")
    return results


def inference_with_saved_models_mode():
    """Inference using saved models"""
    print("🔮 INFERENCE WITH SAVED MODELS")
    print("=" * 40)

    predictor = HardwarePredictor(clean_previous=False)

    # Check for saved models
    area_models = [f for f in os.listdir('.') if f.startswith('hardware_predictor_area_model')]
    power_models = [f for f in os.listdir('.') if f.startswith('hardware_predictor_total_power_model')]

    if not area_models or not power_models:
        print("❌ No saved models found. Run training mode first.")
        return None

    # Use latest models
    area_model = sorted(area_models)[-1]
    power_model = sorted(power_models)[-1]

    # New samples for prediction
    new_samples = [
        {'Dataflow': 'OS', 'Total Number of Multipliers': 1024, 'R': 32, 'C': 32, 'A': 1, 'B': 1, 'P': 1, 'Streaming Dimension Size': 256}
    ]

    # Make predictions
    area_predictions = predictor.predict_with_saved_model(area_model, new_samples.copy())
    power_predictions = predictor.predict_with_saved_model(power_model, new_samples.copy())

    # Save inference results with JSON metadata
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")

    # Save CSV results
    inference_results_file = f"hardware_predictor_inference_results_{timestamp}.csv"
    with open(inference_results_file, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        headers = ['Sample_ID', 'Dataflow', 'Total_Multipliers', 'R', 'C', 'A', 'B', 'P',
                   'Streaming_Size', 'Predicted_Area', 'Predicted_Power']
        writer.writerow(headers)

        for i, sample in enumerate(new_samples):
            writer.writerow([
                i + 1, sample['Dataflow'], sample['Total Number of Multipliers'],
                sample['R'], sample['C'], sample['A'], sample['B'], sample['P'],
                sample['Streaming Dimension Size'], area_predictions[i], power_predictions[i]
            ])

    # Save JSON metadata
    inference_summary = {
        'timestamp': datetime.now().isoformat(),
        'inference_session': timestamp,
        'models_used': {'area_model': area_model, 'power_model': power_model},
        'samples_processed': len(new_samples),
        'input_samples': new_samples,
        'predictions': {'Area': area_predictions, 'Total Power': power_predictions}
    }

    with open(f"hardware_predictor_inference_summary_{timestamp}.json", 'w') as f:
        json.dump(inference_summary, f, indent=2)

    print(f"\n📊 PREDICTIONS:")
    for i, sample in enumerate(new_samples):
        print(f"Sample {i+1} ({sample['Dataflow']}): Area={area_predictions[i]:.1f}, Power={power_predictions[i]:.1f}")

    return {'Area': area_predictions, 'Total Power': power_predictions}


def main():
    """Main execution controller"""
    print("🤖 HARDWARE PREDICTOR v2.1 - OPTIMIZED")
    print("=" * 50)

    # Set mode: "1" for training+inference, "2" for inference only
    mode = "2"

    if mode == "1":
        return train_and_inference_mode()
    elif mode == "2":
        return inference_with_saved_models_mode()
    else:
        print("Invalid mode")
        return None


if __name__ == "__main__":
    results = main()