CREATE TABLE Staff (
    EmpID INT PRIMARY KEY,
    name VARCHAR(30),
    salary DECIMAL(10, 2),
    vacation_days INT,
    vacation_leave BIT,
    supervisorFK INT,
    departmentFK CHAR(2),
    position VARCHAR(20) 
    CHECK (position IN ('Director', 'Manager', 'Supervisor', 'Assistant'))
);

CONSTRAINT [FK_Staff_Department] FOREIGN KEY ([departmentFK]) REFERENCES [dbo].[Department] ([DepartmentID]) 
ON DELETE CASCADE 
ON UPDATE CASCADE,

CONSTRAINT [FK_Staff_Supervisor] FOREIGN KEY ([SupervisorFK]) REFERENCES [dbo].[Staff] ([EmpID]) 
ON DELETE CASCADE 
ON UPDATE CASCADE
