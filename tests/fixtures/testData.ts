/**
 * Test fixtures for Help Me Modernize testing
 * Provides sample legacy code files and expected outputs
 */

export const sampleFiles = {
  cobol: {
    simple: `IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-GREETING PIC X(20) VALUE 'HELLO WORLD'.

PROCEDURE DIVISION.
DISPLAY WS-GREETING.
STOP RUN.`,

    complex: `IDENTIFICATION DIVISION.
PROGRAM-ID. PAYROLL-CALC.
AUTHOR. LEGACY SYSTEM.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT EMPLOYEE-FILE ASSIGN TO "EMPLOYEE.DAT"
        ORGANIZATION IS SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD EMPLOYEE-FILE.
01 EMPLOYEE-RECORD.
   05 EMP-ID       PIC 9(6).
   05 EMP-NAME     PIC X(30).
   05 EMP-SALARY   PIC 9(8)V99.

WORKING-STORAGE SECTION.
01 WS-TOTAL-SALARY  PIC 9(10)V99 VALUE ZERO.
01 WS-EMPLOYEE-COUNT PIC 9(4) VALUE ZERO.

PROCEDURE DIVISION.
MAIN-PARA.
    OPEN INPUT EMPLOYEE-FILE
    PERFORM READ-EMPLOYEE-PARA UNTIL EOF
    CLOSE EMPLOYEE-FILE
    DISPLAY "TOTAL EMPLOYEES: " WS-EMPLOYEE-COUNT
    DISPLAY "TOTAL PAYROLL: " WS-TOTAL-SALARY
    STOP RUN.

READ-EMPLOYEE-PARA.
    READ EMPLOYEE-FILE
        AT END SET EOF TO TRUE
        NOT AT END
            ADD 1 TO WS-EMPLOYEE-COUNT
            ADD EMP-SALARY TO WS-TOTAL-SALARY
    END-READ.`
  },

  java: {
    legacy: `package com.government.legacy;

import java.util.*;
import java.io.*;

/**
 * Legacy Employee Management System
 * Written in Java 1.4 style
 */
public class EmployeeManager {
    private Vector employees = new Vector();
    
    public void addEmployee(String id, String name, double salary) {
        Hashtable employee = new Hashtable();
        employee.put("id", id);
        employee.put("name", name);
        employee.put("salary", new Double(salary));
        employees.add(employee);
    }
    
    public void processPayroll() {
        for (int i = 0; i < employees.size(); i++) {
            Hashtable emp = (Hashtable) employees.get(i);
            String name = (String) emp.get("name");
            Double salary = (Double) emp.get("salary");
            System.out.println("Processing: " + name + " Salary: " + salary);
        }
    }
}`,

    modern: `package com.government.modern;

import java.util.List;
import java.util.ArrayList;
import java.math.BigDecimal;

/**
 * Modern Employee Management System
 * Uses modern Java practices
 */
public class EmployeeManager {
    private final List<Employee> employees = new ArrayList<>();
    
    public void addEmployee(Employee employee) {
        employees.add(employee);
    }
    
    public void processPayroll() {
        employees.forEach(employee -> 
            System.out.printf("Processing: %s Salary: %s%n", 
                employee.getName(), employee.getSalary()));
    }
    
    public record Employee(String id, String name, BigDecimal salary) {}
}`
  },

  sql: {
    legacy: `-- Legacy government database queries
-- Written for older SQL standards

SELECT e.emp_id, e.emp_name, e.salary, d.dept_name
FROM employee e, department d
WHERE e.dept_id = d.dept_id
  AND e.salary > 50000
  AND d.dept_name = 'IT'
ORDER BY e.salary DESC;

-- Vulnerable to SQL injection
DECLARE @emp_id VARCHAR(10)
SET @emp_id = 'user_input'
EXEC('SELECT * FROM employee WHERE emp_id = ''' + @emp_id + '''')`,

    modern: `-- Modern SQL with proper joins and parameterization
SELECT 
    e.emp_id,
    e.emp_name,
    e.salary,
    d.dept_name
FROM employee e
INNER JOIN department d ON e.dept_id = d.dept_id
WHERE e.salary > @min_salary
  AND d.dept_name = @department_name
ORDER BY e.salary DESC;

-- Parameterized query (safe from SQL injection)
SELECT * FROM employee WHERE emp_id = @emp_id;`
  }
};

export const expectedAnalysis = {
  cobol: {
    documentation: {
      summary: "COBOL program for payroll calculation",
      businessLogic: ["Employee record processing", "Salary calculations", "File I/O operations"],
      complexity: "Medium",
      maintainability: "Low - Legacy COBOL structure"
    },
    security: {
      vulnerabilities: ["File access without validation", "No input sanitization"],
      riskLevel: "Medium"
    },
    modernization: {
      suggestions: ["Convert to Java/C#", "Add input validation", "Use modern database"],
      effort: "High"
    }
  },

  java: {
    documentation: {
      summary: "Legacy Java employee management using outdated collections",
      businessLogic: ["Employee data management", "Payroll processing"],
      complexity: "Low",
      maintainability: "Low - Uses deprecated Vector/Hashtable"
    },
    security: {
      vulnerabilities: ["Raw types", "No input validation", "Thread safety issues"],
      riskLevel: "High"
    },
    modernization: {
      suggestions: ["Use generics", "Replace Vector with ArrayList", "Add proper encapsulation"],
      effort: "Low"
    }
  }
};

export const mockMCPTools = {
  analyzeCode: {
    name: "analyze_legacy_code",
    description: "Analyze legacy code for documentation and modernization",
    inputSchema: {
      type: "object",
      properties: {
        code: { type: "string" },
        language: { type: "string" },
        analysisType: { type: "string" }
      }
    }
  },

  uploadFile: {
    name: "upload_file",
    description: "Upload and process a legacy code file",
    inputSchema: {
      type: "object",
      properties: {
        filename: { type: "string" },
        content: { type: "string" },
        encoding: { type: "string" }
      }
    }
  }
};
