CREATE TABLE DEPARTMENT (
    DepartmentID CHAR(2) PRIMARY KEY,
    name VARCHAR(50),
    building VARCHAR(20),
    floor INT,
    rooms INT,
    managerFK INT,
    phone VARCHAR(10)
);

--Department:
CONSTRAINT [FK_Department_Manager] FOREIGN KEY ([ManagerFK]) REFERENCES [dbo].[Staff] ([EmpID])
ON DELETE CASCADE 
ON UPDATE CASCADE