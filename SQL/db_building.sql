CREATE DATABASE website_users;

USE website_users;

CREATE TABLE users (
    PlayerID INT PRIMARY KEY,
    Currency VARCHAR(10),
    Player_Type VARCHAR(30),
    Bonus_Excluded VARCHAR(3),
    Closed VARCHAR(3),
    Category_of_Player VARCHAR(30)
);

LOAD DATA INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/List_Of_Players.csv'
INTO TABLE users
FIELDS TERMINATED BY ';'
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;

CREATE TABLE predictions (
	PredictionID INT AUTO_INCREMENT PRIMARY KEY,
    PlayerID INT,
    EventID INT,
    Event_Name VARCHAR(100),
    Prediction VARCHAR(30),
    Points INT,
    Created_at DATETIME,
    Changed_at DATETIME,
    Round_Number INT,
    FOREIGN KEY (PlayerID) REFERENCES users(PlayerID)
);

LOAD DATA INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/Round 1.csv'
INTO TABLE predictions
FIELDS TERMINATED BY ';'
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS
(PlayerID, EventID, Event_Name, Prediction, Points, Created_at, Changed_at)
SET Round_Number = 1;

LOAD DATA INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/Round 2.csv'
INTO TABLE predictions
FIELDS TERMINATED BY ';'
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS
(PlayerID, EventID, Event_Name, Prediction, Points, Created_at, Changed_at)
SET Round_Number = 2;

LOAD DATA INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/Round 3.csv'
INTO TABLE predictions
FIELDS TERMINATED BY ';'
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS
(PlayerID, EventID, Event_Name, Prediction, Points, Created_at, Changed_at)
SET Round_Number = 3;

LOAD DATA INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/Round 4.csv'
INTO TABLE predictions
FIELDS TERMINATED BY ';'
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS
(PlayerID, EventID, Event_Name, Prediction, Points, Created_at, Changed_at)
SET Round_Number = 4;

LOAD DATA INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/Round 5.csv'
INTO TABLE predictions
FIELDS TERMINATED BY ';'
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS
(PlayerID, EventID, Event_Name, Prediction, Points, Created_at, Changed_at)
SET Round_Number = 5;

LOAD DATA INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/Round 6.csv'
INTO TABLE predictions
FIELDS TERMINATED BY ';'
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS
(PlayerID, EventID, Event_Name, Prediction, Points, Created_at, Changed_at)
SET Round_Number = 6;

LOAD DATA INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/Round 7.csv'
INTO TABLE predictions
FIELDS TERMINATED BY ';'
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS
(PlayerID, EventID, Event_Name, Prediction, Points, Created_at, Changed_at)
SET Round_Number = 7;

LOAD DATA INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/Round 8.csv'
INTO TABLE predictions
FIELDS TERMINATED BY ';'
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS
(PlayerID, EventID, Event_Name, Prediction, Points, Created_at, Changed_at)
SET Round_Number = 8;

LOAD DATA INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/Round 9.csv'
INTO TABLE predictions
FIELDS TERMINATED BY ';'
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS
(PlayerID, EventID, Event_Name, Prediction, Points, Created_at, Changed_at)
SET Round_Number = 9;

LOAD DATA INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/Round 10.csv'
INTO TABLE predictions
FIELDS TERMINATED BY ';'
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS
(PlayerID, EventID, Event_Name, Prediction, Points, Created_at, Changed_at)
SET Round_Number = 10;

LOAD DATA INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/Round 11.csv'
INTO TABLE predictions
FIELDS TERMINATED BY ';'
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS
(PlayerID, EventID, Event_Name, Prediction, Points, Created_at, Changed_at)
SET Round_Number = 11;