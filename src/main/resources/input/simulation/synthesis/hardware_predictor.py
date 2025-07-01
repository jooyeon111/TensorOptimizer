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
    """Hardware Prediction System with Feature Engineering - Optimized Version"""

    def __init__(self, save_to_current_dir: bool = True, clean_previous: bool = True):
        # self.dataflow_mapping = {}
        self.dataflow_mapping = {
            'IS': 0,  # Input Stationary
            'OS': 1,  # Output Stationary
            'WS': 2   # Weight Stationary
        }
        self.feature_stats = {}
        self.save_dir = "." if save_to_current_dir else "model_artifacts"

        if not save_to_current_dir:
            os.makedirs(self.save_dir, exist_ok=True)

        if clean_previous and save_to_current_dir:
            self._cleanup_files()

        # Updated feature columns with engineered features
        self.feature_columns = [
            'Dataflow_encoded',
            'Total Number of Multipliers', 'R', 'C', 'A', 'B', 'P',
            'Streaming Dimension Size',
            'R_C_interaction',           # New: Interaction term R*C
            'A_B_P_interaction',        # New: Interaction term A*B*P
            'R_A_P_interaction',        # New: Interaction term A*B*P
            'C_B_P_interaction',        # New: Interaction term A*B*P
            'R_C_A_B_interaction',           # New: Interaction term R*C
            'Streaming_size_log',       # New: Log transform of streaming size
            'Power_complexity_factor'  # ADD THIS LINE

        ]

        self.configs = {
            'Area': {'sample_size': 25, 'alpha': 0.01, 'ensemble_size': 5, 'n_trials': 12},
            'Total Power': {'sample_size': 25, 'alpha': 0.001, 'ensemble_size': 10, 'n_trials': 12}
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

    def engineer_features(self, dataset: List[Dict]) -> List[Dict]:
        """Add engineered features to dataset"""
        engineered_data = []
        for row in dataset:
            new_row = row.copy()
            # Interaction terms
            new_row['R_C_interaction'] = new_row['R'] * new_row['C']

            new_row['A_B_P_interaction'] = new_row['A'] * new_row['B'] * new_row['P']
            new_row['R_A_P_interaction'] = new_row['R'] * new_row['A'] * new_row['P']
            new_row['C_B_P_interaction'] = new_row['C'] * new_row['B'] * new_row['P']

            new_row['R_C_A_B_interaction'] = new_row['R'] * new_row['C'] * new_row['A'] * new_row['B']

            new_row['Streaming_size_log'] = math.log2(new_row['Streaming Dimension Size'])
            new_row['Power_complexity_factor'] = (new_row['Total Number of Multipliers'] * new_row['Streaming Dimension Size']) ** 0.5

            engineered_data.append(new_row)
        return engineered_data

    def preprocess_data(self, train_path: str, val_path: str, test_path: str):
        """Load, preprocess, and engineer features for all datasets"""
        train_data = self.load_csv(train_path)
        val_data = self.load_csv(val_path)
        test_data = self.load_csv(test_path)

        # Create dataflow mapping
        # all_data = train_data + val_data + test_data
        # unique_dataflows = list(set(row['Dataflow'] for row in all_data))
        # self.dataflow_mapping = {df: i for i, df in enumerate(unique_dataflows)}

        # Encode dataflows
        for dataset in [train_data, val_data, test_data]:
            for row in dataset:
                row['Dataflow_encoded'] = self.dataflow_mapping[row['Dataflow']]

        # Engineer features
        train_data = self.engineer_features(train_data)
        val_data = self.engineer_features(val_data)
        test_data = self.engineer_features(test_data)

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
            headers = ['Dataflow', 'RxCxAxBxP', 'True Area', 'Predicted Area', 'True Power', 'Predicted Power', 'Area Error Percentage', 'Power Error Percentage']
            writer.writerow(headers)

            for i, val_row in enumerate(val_data):
                # Create RxCxAxBxP format
                dimension_str = f"{val_row['R']}x{val_row['C']}x{val_row['A']}x{val_row['B']}x{val_row['P']}"

                # Get predictions
                area_pred = results['Area']['best_result']['predictions'][i] if 'best_result' in results['Area'] else 0
                power_pred = results['Total Power']['best_result']['predictions'][i] if 'best_result' in results['Total Power'] else 0

                # Calculate error percentages
                area_error = abs((val_row['Area'] - area_pred) / val_row['Area']) * 100 if val_row['Area'] != 0 else 0
                power_error = abs((val_row['Total Power'] - power_pred) / val_row['Total Power']) * 100 if val_row['Total Power'] != 0 else 0

                row_data = [
                    val_row['Dataflow'],
                    dimension_str,
                    val_row['Area'],
                    round(area_pred, 2),
                    val_row['Total Power'],
                    round(power_pred, 2),
                    round(area_error, 2),
                    round(power_error, 2)
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

        # Engineer features for new samples
        new_samples = self.engineer_features(new_samples)

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

        # Engineer features for new samples
        new_samples = self.engineer_features(new_samples)

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
            # CHANGE: Keep only input features and predicted values
            headers = ['Sample_ID', 'Dataflow', 'Total_Multipliers', 'R', 'C', 'A', 'B', 'P',
                       'Streaming_Size', 'Predicted_Area', 'Predicted_Power']
            writer.writerow(headers)

            for i, sample in enumerate(new_samples):
                writer.writerow([
                    i + 1, sample['Dataflow'], sample['Total Number of Multipliers'],
                    sample['R'], sample['C'], sample['A'], sample['B'], sample['P'],
                    sample['Streaming Dimension Size'],
                    predictions['Area'][i], predictions['Total Power'][i]  # Only predicted values
                ])

        return predictions

    def evaluate_test_set(self, train_data: List[Dict], test_data: List[Dict]) -> Dict[str, Any]:
        """Evaluate model performance on test set and calculate MAPE"""
        print("\n📊 TEST SET EVALUATION")
        print("-" * 30)

        # Check if test data has ground truth values
        has_ground_truth = all('Area' in sample and 'Total Power' in sample for sample in test_data)

        if not has_ground_truth:
            print("⚠️  Test data does not contain ground truth values (Area, Total Power)")
            print("   Only predictions will be generated.")
            return self.predict_new_samples(train_data, test_data)

        # Engineer features for test samples
        test_samples = self.engineer_features(test_data)

        # Encode test samples
        for sample in test_samples:
            sample['Dataflow_encoded'] = self.dataflow_mapping.get(sample['Dataflow'], 0)

        test_results = {}
        for target in ['Area', 'Total Power']:
            print(f"\n🎯 Evaluating {target} on test set...")

            # Get predictions
            predictions, _ = self.ensemble_predict(train_data, test_samples, target, self.configs[target])

            # Calculate metrics
            true_values = [sample[target] for sample in test_samples]
            metrics = self.calculate_metrics(true_values, predictions)

            test_results[target] = {
                'predictions': predictions,
                'true_values': true_values,
                'metrics': metrics
            }

            print(f"   Test MAPE: {metrics['MAPE']:.2f}%")
            print(f"   Test R²: {metrics['R2']:.3f}")
            print(f"   Test RMSE: {metrics['RMSE']:.2f}")

        # Save test results
        self._save_test_results(test_samples, test_results)

        return test_results

    def _save_test_results(self, test_data: List[Dict], test_results: Dict[str, Any]):
        """Save test evaluation results"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")

        # Save detailed test predictions in requested format
        with open("hardware_predictor_test_evaluation.csv", 'w', newline='') as csvfile:
            writer = csv.writer(csvfile)
            headers = ['Dataflow', 'RxCxAxBxP', 'True Area', 'Predicted Area', 'True Power', 'Predicted Power', 'Area Error Percentage', 'Power Error Percentage']
            writer.writerow(headers)

            for i, sample in enumerate(test_data):
                # Create RxCxAxBxP format
                dimension_str = f"{sample['R']}x{sample['C']}x{sample['A']}x{sample['B']}x{sample['P']}"

                # Calculate error percentages
                area_error = abs((sample['Area'] - test_results['Area']['predictions'][i]) / sample['Area']) * 100 if sample['Area'] != 0 else 0
                power_error = abs((sample['Total Power'] - test_results['Total Power']['predictions'][i]) / sample['Total Power']) * 100 if sample['Total Power'] != 0 else 0

                writer.writerow([
                    sample['Dataflow'],
                    dimension_str,
                    sample['Area'],
                    round(test_results['Area']['predictions'][i], 2),
                    sample['Total Power'],
                    round(test_results['Total Power']['predictions'][i], 2),
                    round(area_error, 2),
                    round(power_error, 2)
                ])

        # Save test metrics summary
        test_summary = {
            'timestamp': datetime.now().isoformat(),
            'test_session': timestamp,
            'test_metrics': {
                target: test_results[target]['metrics']
                for target in ['Area', 'Total Power']
            },
            'sample_count': len(test_data)
        }

        with open("hardware_predictor_test_metrics.json", 'w') as f:
            json.dump(test_summary, f, indent=2)

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

    # Evaluate on test set and calculate MAPE
    test_results = predictor.evaluate_test_set(train_data, test_data)

    if 'Area' in test_results and 'metrics' in test_results['Area']:
        print(f"\n📈 FINAL TEST SET PERFORMANCE:")
        print(f"   Area - MAPE: {test_results['Area']['metrics']['MAPE']:.2f}%, R²: {test_results['Area']['metrics']['R2']:.3f}")
        print(f"   Power - MAPE: {test_results['Total Power']['metrics']['MAPE']:.2f}%, R²: {test_results['Total Power']['metrics']['R2']:.3f}")
    else:
        print(f"\n📊 TEST DATA RESULTS (Predictions Only):")
        for i, sample in enumerate(test_data):
            print(f"Sample {i+1} ({sample['Dataflow']}): Area={test_results['Area'][i]:.1f}, Power={test_results['Total Power'][i]:.1f}")

    print(f"\n✅ Training and evaluation completed - models and results saved to current directory")

    return results, test_results

def main():
    """Main execution controller"""
    print("🤖 HARDWARE PREDICTOR v2.2 - OPTIMIZED WITH FEATURE ENGINEERING")
    print("=" * 50)

    # Set mode: "1" for training+inference, "2" for inference only
    results, test_results = train_and_inference_mode()
    return results, test_results

if __name__ == "__main__":
    results, test_results = main()