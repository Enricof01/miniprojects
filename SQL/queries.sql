--a) Durchschnittsgehalt aller Angestellten
SELECT AVG(Salary) AS durchschnitt
FROM Staff

--b) Höchstes Gehalt aller Angestellten
SELECT MAX(Salary) AS max FROM Staff;

--c) Anzahl Mitarbeiter mit Gehalt > 50 000
SELECT COUNT(*) AS anzahl_mitarbeiter
FROM Staff
WHERE Salary > 50000;

--d) Durchschnittsgehalt aller Abteilungen
SELECT AVG(Salary) AS durchschnittsgehalt, departmentFK
FROM Staff
GROUP BY departmentFK;

--e) Kleinstes Gehalt aller Mitarbeiter die im Urlaub sind
SELECT MIN(Salary) AS MinGehalt
FROM Staff
WHERE Vacation_Leave = 1;

--f) Anzahl Mitarbeiter pro Position
SELECT count(*) AS Anzahl, Position
FROM Staff
GROUP BY Position;

--g) Durchschnittsgehalt gruppiert nach Abteilung und Räume
SELECT AVG(Salary) AS Durchschnittsgehalt, Rooms
From Staff, Department
GROUP BY DepartmentID, Rooms;

--h) 
SELECT COUNT(*) AS Anzahl, departmentFK, Rooms
FROM Staff, Department
GROUP BY departmentFK, Rooms
HAVING COUNT(*) > 4;

--i) Welche Assistenten verdienen mehr als ihr vorgesetzter Supervisor
SELECT s1.Name, s1.Position FROM Staff s1, Staff s2
WHERE s1.Salary > s2.Salary AND s1.SupervisorFK = s2.EmpID AND s1.Position = 'Assistant';

--j) Welche Position am wenigsten im Urlaub
SELECT TOP 1  COUNT(*) AS Anzahl, Position
FROM Staff
WHERE Vacation_Leave = 0
GROUP BY  Position
ORDER BY Anzahl DESC;

--k) prozentualer Gehaltsvergleich
SELECT (
    (manager_avg - supervisor_avg) / supervisor_avg
) * 100 AS prozentuale_Diff

FROM (
    SELECT 
    AVG(CASE WHEN position = 'Manager' THEN Salary END) AS manager_avg,
    AVG(CASE WHEN position = 'Supervisor' THEN Salary END) AS supervisor_avg
    FROM Staff
) AS gehalt_vergleich;


--l) In welcher Abteilung verdienen die Angestellten in Summe am meisten (Direktor wird ausgeschlossen)
SELECT  TOP 1 DepartmentID, SUM(Salary) AS Summe, DepartmentID
FROM Department, Staff
WHERE DepartmentID = departmentFK AND NOT Position = 'Director'
GROUP BY DepartmentID
ORDER BY Summe DESC;

--m) Wahrheitstabelle mit NULL, 1 und 0 Werten

SELECT
A,
B,
CASE 
    WHEN A = 1 AND B = 1 THEN 1
    WHEN A IS NULL OR B IS NULL THEN NULL
    ELSE 0
END AS A_AND_B,

CASE
    WHEN A = 1 OR B = 1 THEN 1
    WHEN A IS NULL OR B IS NULL THEN NULL
ELSE 0
END AS A_OR_B,

    CASE
    WHEN A = 1 THEN 0
    WHEN A = 0 THEN 1
    ELSE NULL
    END AS NOT_A
FROM logik;

//n)




