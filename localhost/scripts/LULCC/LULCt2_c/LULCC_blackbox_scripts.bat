@echo off 
title "Land Use/Cover Change submodel in Mofuss v1.0"

@echo Please wait, it can take some minutes until this script finds the needed files.

@echo off & setLocal EnableDELAYedeXpansion

cd \
for %%d in (c d e f g h i j k l m n o p q r s t u v w x y z) do (
	if exist %%d: (
		for /f "tokens=* delims= " %%a in ('dir/b/s %%d:\DinamicaConsole.exe 2^>nul') do (
        		"%%a" -processors 0 -log-level 4 "%~dp01_Matrix_gain_win241.egoml"

			"%%a" -processors 0 -log-level 4 "%~dp01_Matrix_loss_win241.egoml"

			"%%a" -processors 0 -log-level 4 "%~dp02_Distance_calc_win241.egoml"

			"%%a" -processors 0 -log-level 4 "%~dp03_Ranges_gain_win241.egoml" 

			"%%a" -processors 0 -log-level 4 "%~dp03_Ranges_loss_win241.egoml" 

			"%%a" -processors 0 -log-level 4 "%~dp04_Weights_gain_win241.egoml" 

			"%%a" -processors 0 -log-level 4 "%~dp04_Weights_loss_win241.egoml" 

			"%%a" -processors 0 -log-level 4 "%~dp05_Correlation_gain_win241.egoml" 

			"%%a" -processors 0 -log-level 4 "%~dp05_Correlation_loss_win241.egoml" 

			"%%a" -processors 0 -log-level 4 "%~dp06_Probability_gain_win241.egoml" 

			"%%a" -processors 0 -log-level 4 "%~dp06_Probability_loss_win241.egoml"  

			"%%a" -processors 0 -log-level 4 "%~dp07_Simulation_gain_win241.egoml" 

			"%%a" -processors 0 -log-level 4 "%~dp07_Simulation_loss_win241.egoml" 

			"%%a" -processors 0 -log-level 4 "%~dp08_Validation_gain_win241.egoml" 

			"%%a" -processors 0 -log-level 4 "%~dp08_Validation_loss_win241.egoml" 

		)
	)
)
	