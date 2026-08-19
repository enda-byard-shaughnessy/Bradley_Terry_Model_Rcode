# Bradley-Terry model for ATP match prediction

Research internship project, Trinity College Dublin.

Project webpage: https://enda-byard-shaughnessy.github.io/Bradley_Terry_Model_Tennis/

## Scripts

| File | Purpose |
|---|---|
| `hyperparameter_determining_file.R` | Determining model hyperparameters |
| `SE_calculation_via_hessian.R` | Standard errors for the estimated player abilities via optim's Hessian calculation |
| `model_accuracy_check.R` | Model accuracy check for fixed hyperparameter values |
| `log_loss_check.R` | Log loss check for fixed hyperparameter values |
| `predictive_accuracy_unseen_data.R` | Model accuracy on unseen data from the first half 2026 (02/01/2026 - 21/06/2026) |

Data: ATP match results from 2024,2025,2026, from https://github.com/JeffSackmann/tennis_atp. Not included in this repo.
