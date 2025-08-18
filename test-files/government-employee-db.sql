-- Legacy Government Employee Database Schema
-- Original Creation: 1987
-- Last Major Update: 2003
-- Current Status: PRODUCTION - DO NOT MODIFY WITHOUT APPROVAL
-- 
-- WARNING: This database contains 30+ years of accumulated business logic
-- Many procedures contain hardcoded values that reflect old legislation
-- Some constraints may no longer reflect current policy
--
-- KNOWN ISSUES:
-- 1. SSN field should be encrypted (compliance issue)
-- 2. Several indexes are missing (performance issue)  
-- 3. Salary table has not been updated since 2008
-- 4. Some triggers contain bugs but are too risky to fix

-- =====================================================
-- EMPLOYEE MASTER TABLE
-- Core employee information - over 2 million records
-- =====================================================

CREATE TABLE EMPLOYEE_MASTER (
    EMP_ID              VARCHAR2(6) PRIMARY KEY,
    SSN                 VARCHAR2(11) NOT NULL,  -- SECURITY ISSUE: Not encrypted
    LAST_NAME           VARCHAR2(30) NOT NULL,
    FIRST_NAME          VARCHAR2(20) NOT NULL,
    MIDDLE_INITIAL      CHAR(1),
    GRADE               NUMBER(2) NOT NULL,
    STEP                NUMBER(2) DEFAULT 1,
    JOB_SERIES          VARCHAR2(4),
    POSITION_TITLE      VARCHAR2(50),
    DEPARTMENT_CODE     VARCHAR2(4),
    AGENCY_CODE         VARCHAR2(3),
    HIRE_DATE           DATE NOT NULL,
    RETIREMENT_DATE     DATE,
    STATUS_CODE         CHAR(1) DEFAULT 'A',  -- A=Active, R=Retired, T=Terminated
    SALARY_ANNUAL       NUMBER(8,2),
    PAY_PLAN            VARCHAR2(3) DEFAULT 'GS',
    WORK_SCHEDULE       VARCHAR2(10) DEFAULT 'FULL_TIME',
    LOCATION_CODE       VARCHAR2(6),
    SUPERVISOR_ID       VARCHAR2(6),
    CREATED_DATE        DATE DEFAULT SYSDATE,
    MODIFIED_DATE       DATE DEFAULT SYSDATE,
    MODIFIED_BY         VARCHAR2(20),
    
    -- Legacy constraints that nobody dares to change
    CONSTRAINT CHK_EMP_GRADE CHECK (GRADE BETWEEN 1 AND 15),
    CONSTRAINT CHK_EMP_STEP CHECK (STEP BETWEEN 1 AND 10),
    CONSTRAINT CHK_EMP_STATUS CHECK (STATUS_CODE IN ('A', 'R', 'T', 'S')),
    CONSTRAINT FK_EMP_SUPERVISOR FOREIGN KEY (SUPERVISOR_ID) REFERENCES EMPLOYEE_MASTER(EMP_ID)
);

-- Index that was supposed to be temporary but became permanent
CREATE INDEX IDX_EMP_SSN ON EMPLOYEE_MASTER(SSN);
CREATE INDEX IDX_EMP_NAME ON EMPLOYEE_MASTER(LAST_NAME, FIRST_NAME);
CREATE INDEX IDX_EMP_DEPT ON EMPLOYEE_MASTER(DEPARTMENT_CODE, AGENCY_CODE);

-- =====================================================
-- BENEFITS ENROLLMENT TABLE  
-- Tracks employee benefit elections
-- =====================================================

CREATE TABLE BENEFITS_ENROLLMENT (
    EMP_ID              VARCHAR2(6) NOT NULL,
    PLAN_YEAR           NUMBER(4) NOT NULL,
    HEALTH_PLAN_CODE    VARCHAR2(10),
    DENTAL_PLAN_CODE    VARCHAR2(10),
    VISION_PLAN_CODE    VARCHAR2(10),
    LIFE_INSURANCE_AMT  NUMBER(10,2),
    FLEX_SPENDING_AMT   NUMBER(6,2),
    ENROLLMENT_DATE     DATE NOT NULL,
    EFFECTIVE_DATE      DATE NOT NULL,
    END_DATE            DATE,
    FAMILY_SIZE         NUMBER(2) DEFAULT 1,
    
    PRIMARY KEY (EMP_ID, PLAN_YEAR),
    CONSTRAINT FK_BENEFITS_EMP FOREIGN KEY (EMP_ID) REFERENCES EMPLOYEE_MASTER(EMP_ID)
);

-- =====================================================
-- PAYROLL HISTORY TABLE
-- 15+ years of payroll data - HUGE table
-- WARNING: Contains performance issues
-- =====================================================

CREATE TABLE PAYROLL_HISTORY (
    EMP_ID              VARCHAR2(6) NOT NULL,
    PAY_PERIOD_END      DATE NOT NULL,
    HOURS_WORKED        NUMBER(5,2),
    OVERTIME_HOURS      NUMBER(5,2),
    GROSS_PAY           NUMBER(8,2),
    FEDERAL_TAX         NUMBER(8,2),
    STATE_TAX           NUMBER(8,2),
    FICA_TAX            NUMBER(8,2),
    RETIREMENT_DEDUCT   NUMBER(8,2),
    HEALTH_DEDUCT       NUMBER(8,2),
    OTHER_DEDUCTS       NUMBER(8,2),
    NET_PAY             NUMBER(8,2),
    CREATED_DATE        DATE DEFAULT SYSDATE,
    
    PRIMARY KEY (EMP_ID, PAY_PERIOD_END),
    CONSTRAINT FK_PAYROLL_EMP FOREIGN KEY (EMP_ID) REFERENCES EMPLOYEE_MASTER(EMP_ID)
);

-- Missing index that causes performance problems
-- CREATE INDEX IDX_PAYROLL_DATE ON PAYROLL_HISTORY(PAY_PERIOD_END); -- TODO: Add during maintenance window

-- =====================================================
-- LEGACY STORED PROCEDURES
-- Contains critical business logic that cannot be changed
-- =====================================================

-- Procedure to calculate overtime pay
-- Written in 1995, contains hardcoded overtime rules
CREATE OR REPLACE PROCEDURE CALC_OVERTIME_PAY (
    p_emp_id IN VARCHAR2,
    p_hours_worked IN NUMBER,
    p_overtime_pay OUT NUMBER
) AS
    v_hourly_rate NUMBER;
    v_grade NUMBER;
    v_overtime_hours NUMBER;
BEGIN
    -- Get employee grade to determine hourly rate
    SELECT GRADE INTO v_grade 
    FROM EMPLOYEE_MASTER 
    WHERE EMP_ID = p_emp_id;
    
    -- Hardcoded pay rates from 1995 - NEVER UPDATED
    -- This is a known bug but too risky to fix
    CASE 
        WHEN v_grade <= 5 THEN v_hourly_rate := 12.50;
        WHEN v_grade <= 9 THEN v_hourly_rate := 18.75;
        WHEN v_grade <= 12 THEN v_hourly_rate := 28.50;
        WHEN v_grade <= 15 THEN v_hourly_rate := 42.75;
        ELSE v_hourly_rate := 15.00;  -- Default fallback
    END CASE;
    
    -- Calculate overtime hours (over 40 per week)
    IF p_hours_worked > 40 THEN
        v_overtime_hours := p_hours_worked - 40;
    ELSE
        v_overtime_hours := 0;
    END IF;
    
    -- Overtime rate is 1.5x regular rate
    p_overtime_pay := v_overtime_hours * v_hourly_rate * 1.5;
    
EXCEPTION
    WHEN NO_DATA_FOUND THEN
        p_overtime_pay := 0;
    WHEN OTHERS THEN
        RAISE_APPLICATION_ERROR(-20001, 'Error calculating overtime: ' || SQLERRM);
END CALC_OVERTIME_PAY;
/

-- Function to get employee full name
-- Simple but used everywhere in reports
CREATE OR REPLACE FUNCTION GET_EMPLOYEE_NAME (p_emp_id VARCHAR2) 
RETURN VARCHAR2 AS
    v_full_name VARCHAR2(100);
BEGIN
    SELECT FIRST_NAME || ' ' || 
           CASE WHEN MIDDLE_INITIAL IS NOT NULL 
                THEN MIDDLE_INITIAL || '. ' 
                ELSE '' END ||
           LAST_NAME
    INTO v_full_name
    FROM EMPLOYEE_MASTER
    WHERE EMP_ID = p_emp_id;
    
    RETURN v_full_name;
    
EXCEPTION
    WHEN NO_DATA_FOUND THEN
        RETURN 'UNKNOWN EMPLOYEE';
    WHEN OTHERS THEN
        RETURN 'ERROR';
END GET_EMPLOYEE_NAME;
/

-- Complex benefits calculation procedure
-- Contains 20+ years of policy changes
-- Each IF statement represents a different congressional mandate
CREATE OR REPLACE PROCEDURE CALC_MONTHLY_BENEFITS (
    p_emp_id IN VARCHAR2,
    p_health_cost OUT NUMBER,
    p_dental_cost OUT NUMBER,
    p_total_cost OUT NUMBER
) AS
    v_grade NUMBER;
    v_family_size NUMBER;
    v_years_service NUMBER;
    v_health_plan VARCHAR2(10);
    v_dental_plan VARCHAR2(10);
    v_hire_date DATE;
    
BEGIN
    -- Get employee information
    SELECT e.GRADE, b.FAMILY_SIZE, 
           FLOOR(MONTHS_BETWEEN(SYSDATE, e.HIRE_DATE) / 12) as YEARS_SERVICE,
           b.HEALTH_PLAN_CODE, b.DENTAL_PLAN_CODE, e.HIRE_DATE
    INTO v_grade, v_family_size, v_years_service, 
         v_health_plan, v_dental_plan, v_hire_date
    FROM EMPLOYEE_MASTER e
    JOIN BENEFITS_ENROLLMENT b ON e.EMP_ID = b.EMP_ID
    WHERE e.EMP_ID = p_emp_id
    AND b.PLAN_YEAR = EXTRACT(YEAR FROM SYSDATE);
    
    -- Calculate health insurance cost
    -- Base rates hardcoded from 2008 contract
    CASE v_health_plan
        WHEN 'FEHB_STD' THEN p_health_cost := 287.50;
        WHEN 'FEHB_HIGH' THEN p_health_cost := 425.75;
        WHEN 'FEHB_LOW' THEN p_health_cost := 165.25;
        WHEN 'TRICARE' THEN p_health_cost := 45.00;
        ELSE p_health_cost := 200.00;  -- Default
    END CASE;
    
    -- Family multiplier (policy from 1995)
    IF v_family_size > 1 THEN
        p_health_cost := p_health_cost * 2.5;
    END IF;
    
    -- Senior executive adjustment (added 1998)
    IF v_grade >= 13 THEN
        p_health_cost := p_health_cost * 1.15;
    END IF;
    
    -- Junior employee discount (added 2001)  
    IF v_grade <= 7 THEN
        p_health_cost := p_health_cost * 0.85;
    END IF;
    
    -- Long service discount (added 2001)
    IF v_years_service >= 20 THEN
        p_health_cost := p_health_cost * 0.90;
    END IF;
    
    -- Mysterious multiplier (nobody knows why this exists)
    p_health_cost := p_health_cost * 1.347;
    
    -- Special case for GS-14 hired before 2005 
    -- (Congressional mandate - exact amount specified)
    IF v_grade = 14 AND v_hire_date < DATE '2005-01-01' THEN
        p_health_cost := p_health_cost + 47.83;
    END IF;
    
    -- Calculate dental cost
    IF v_dental_plan IS NOT NULL THEN
        p_dental_cost := 23.45;  -- Base dental cost
        
        -- Family dental coverage
        IF v_family_size > 1 THEN
            p_dental_cost := p_dental_cost + ((v_family_size - 1) * 15.75);
        END IF;
        
        -- Premium dental plan
        IF v_dental_plan = 'PREMIUM' THEN
            p_dental_cost := p_dental_cost * 1.4;
        END IF;
    ELSE
        p_dental_cost := 0;
    END IF;
    
    -- Total cost
    p_total_cost := p_health_cost + p_dental_cost;
    
    -- Round to cents
    p_health_cost := ROUND(p_health_cost, 2);
    p_dental_cost := ROUND(p_dental_cost, 2);
    p_total_cost := ROUND(p_total_cost, 2);
    
EXCEPTION
    WHEN NO_DATA_FOUND THEN
        RAISE_APPLICATION_ERROR(-20002, 'Employee or benefits data not found');
    WHEN OTHERS THEN
        RAISE_APPLICATION_ERROR(-20003, 'Error calculating benefits: ' || SQLERRM);
END CALC_MONTHLY_BENEFITS;
/

-- =====================================================
-- LEGACY VIEWS FOR REPORTING
-- Used by dozens of reports - cannot be changed
-- =====================================================

-- View that joins employee and payroll data
-- Performance issues due to missing indexes
CREATE OR REPLACE VIEW VW_EMPLOYEE_PAYROLL AS
SELECT 
    e.EMP_ID,
    e.LAST_NAME,
    e.FIRST_NAME,
    e.GRADE,
    e.DEPARTMENT_CODE,
    e.AGENCY_CODE,
    p.PAY_PERIOD_END,
    p.GROSS_PAY,
    p.NET_PAY,
    p.FEDERAL_TAX,
    p.STATE_TAX
FROM EMPLOYEE_MASTER e
JOIN PAYROLL_HISTORY p ON e.EMP_ID = p.EMP_ID
WHERE e.STATUS_CODE = 'A';

-- Summary view for management reports
CREATE OR REPLACE VIEW VW_DEPARTMENT_SUMMARY AS
SELECT 
    DEPARTMENT_CODE,
    AGENCY_CODE,
    COUNT(*) as EMPLOYEE_COUNT,
    AVG(SALARY_ANNUAL) as AVG_SALARY,
    MIN(HIRE_DATE) as EARLIEST_HIRE,
    MAX(HIRE_DATE) as LATEST_HIRE
FROM EMPLOYEE_MASTER
WHERE STATUS_CODE = 'A'
GROUP BY DEPARTMENT_CODE, AGENCY_CODE;

-- =====================================================
-- LEGACY TRIGGERS
-- Some contain bugs but are too risky to fix
-- =====================================================

-- Trigger to update employee modified date
-- Simple but critical for audit trail
CREATE OR REPLACE TRIGGER TRG_EMP_MODIFIED
    BEFORE UPDATE ON EMPLOYEE_MASTER
    FOR EACH ROW
BEGIN
    :NEW.MODIFIED_DATE := SYSDATE;
    :NEW.MODIFIED_BY := USER;
END;
/

-- Trigger to validate employee data
-- Contains hardcoded business rules from 1990s
CREATE OR REPLACE TRIGGER TRG_EMP_VALIDATE
    BEFORE INSERT OR UPDATE ON EMPLOYEE_MASTER
    FOR EACH ROW
BEGIN
    -- Validate SSN format (basic check)
    IF NOT REGEXP_LIKE(:NEW.SSN, '^\d{3}-\d{2}-\d{4}$') THEN
        RAISE_APPLICATION_ERROR(-20004, 'Invalid SSN format');
    END IF;
    
    -- Validate hire date (cannot be future)
    IF :NEW.HIRE_DATE > SYSDATE THEN
        RAISE_APPLICATION_ERROR(-20005, 'Hire date cannot be in the future');
    END IF;
    
    -- Validate grade/step combination (old rules)
    IF :NEW.GRADE < 5 AND :NEW.STEP > 8 THEN
        RAISE_APPLICATION_ERROR(-20006, 'Invalid grade/step combination');
    END IF;
    
    -- Other validation rules accumulated over decades...
    
END;
/

-- =====================================================
-- SAMPLE DATA QUERIES
-- Examples of how this legacy system is queried
-- =====================================================

-- Query to find all employees eligible for retirement
-- (Contains hardcoded age calculation logic)
SELECT EMP_ID, GET_EMPLOYEE_NAME(EMP_ID) as FULL_NAME,
       FLOOR(MONTHS_BETWEEN(SYSDATE, HIRE_DATE) / 12) as YEARS_SERVICE
FROM EMPLOYEE_MASTER
WHERE STATUS_CODE = 'A'
AND FLOOR(MONTHS_BETWEEN(SYSDATE, HIRE_DATE) / 12) >= 30
ORDER BY HIRE_DATE;

-- Complex payroll summary query
-- Performance issues with large datasets
SELECT 
    e.DEPARTMENT_CODE,
    e.AGENCY_CODE,
    COUNT(DISTINCT p.EMP_ID) as EMPLOYEE_COUNT,
    SUM(p.GROSS_PAY) as TOTAL_GROSS,
    SUM(p.NET_PAY) as TOTAL_NET,
    AVG(p.GROSS_PAY) as AVG_GROSS
FROM EMPLOYEE_MASTER e
JOIN PAYROLL_HISTORY p ON e.EMP_ID = p.EMP_ID
WHERE p.PAY_PERIOD_END >= ADD_MONTHS(SYSDATE, -12)
AND e.STATUS_CODE = 'A'
GROUP BY e.DEPARTMENT_CODE, e.AGENCY_CODE
ORDER BY TOTAL_GROSS DESC;

-- Query with nested subqueries (typical of legacy SQL)
SELECT emp_id, last_name, first_name, salary_annual
FROM employee_master
WHERE salary_annual > (
    SELECT AVG(salary_annual) 
    FROM employee_master 
    WHERE department_code = 'IT01'
    AND status_code = 'A'
)
AND grade IN (
    SELECT DISTINCT grade
    FROM employee_master
    WHERE hire_date >= DATE '2010-01-01'
    AND status_code = 'A'
)
ORDER BY salary_annual DESC;
