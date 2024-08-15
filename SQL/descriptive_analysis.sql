SELECT *
FROM predictions
WHERE PlayerID IS NULL
	OR EventID IS NULL
    OR Event_Name IS NULL
    OR Prediction IS NULL
    OR Points IS NULL
    OR Created_at IS NULL
    OR Changed_at IS NULL
    OR Round_Number IS NULL;
    
SELECT *
FROM users
WHERE Currency IS NULL
	OR Player_Type IS NULL
    OR Bonus_Excluded IS NULL
    OR Closed IS NULL
    OR Category_of_Player IS NULL;
    
SELECT COUNT(*) AS "Number_of_predictions"
FROM predictions;

SELECT AVG(Points) AS "Average_points_awarded"
FROM predictions;

SELECT AVG(Points) AS "Average_points_awarded_per_round", Round_Number
FROM predictions
GROUP BY Round_Number;