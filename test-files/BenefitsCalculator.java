/**
 * Legacy Government Benefits Management System
 * Original Author: John Smith (Retired 2010)
 * Last Modified: 2008
 * System: Employee Benefits Administration
 * 
 * WARNING: This system contains critical business logic that
 * nobody fully understands anymore. Proceed with caution.
 */

import java.sql.*;
import java.util.*;
import java.text.*;

public class BenefitsCalculator {
    
    // Hardcoded database connection - SECURITY ISSUE
    private static final String DB_URL = "jdbc:oracle:thin:@mainframe:1521:GOVDB";
    private static final String DB_USER = "BENEFITS_USER";
    private static final String DB_PASS = "password123"; // TODO: Fix this security issue
    
    // Magic numbers - nobody knows why these values
    private static final double HEALTH_MULTIPLIER = 1.347;
    private static final double DENTAL_BASE = 23.45;
    private static final int RETIREMENT_FACTOR = 42;
    
    private Connection conn;
    private PreparedStatement employeeStmt;
    private PreparedStatement benefitsStmt;
    
    public BenefitsCalculator() throws SQLException {
        // Legacy JDBC driver setup
        try {
            Class.forName("oracle.jdbc.driver.OracleDriver");
            conn = DriverManager.getConnection(DB_URL, DB_USER, DB_PASS);
            
            // Prepared statements for efficiency (added in 2003)
            employeeStmt = conn.prepareStatement(
                "SELECT emp_id, grade, years_service, family_size, " +
                "health_plan, dental_plan, life_insurance " +
                "FROM employee_master WHERE emp_id = ?"
            );
            
            benefitsStmt = conn.prepareStatement(
                "INSERT INTO monthly_benefits " +
                "(emp_id, calculation_date, health_cost, dental_cost, " +
                "life_cost, retirement_contrib, total_cost) " +
                "VALUES (?, SYSDATE, ?, ?, ?, ?, ?)"
            );
            
        } catch (ClassNotFoundException e) {
            throw new SQLException("Oracle driver not found", e);
        }
    }
    
    /**
     * Calculate monthly benefits for an employee
     * 
     * This method contains 20+ years of accumulated business logic
     * including special cases, exceptions, and congressional mandates
     * 
     * @param employeeId The employee ID (6 digits)
     * @return BenefitsResult containing all calculations
     */
    public BenefitsResult calculateMonthlyBenefits(String employeeId) 
            throws SQLException, BenefitsException {
        
        // Validate employee ID format (legacy validation)
        if (!isValidEmployeeId(employeeId)) {
            throw new BenefitsException("Invalid employee ID format");
        }
        
        // Get employee data
        EmployeeData emp = getEmployeeData(employeeId);
        if (emp == null) {
            throw new BenefitsException("Employee not found: " + employeeId);
        }
        
        // Calculate individual benefit components
        double healthCost = calculateHealthCost(emp);
        double dentalCost = calculateDentalCost(emp);
        double lifeCost = calculateLifeInsurance(emp);
        double retirementContrib = calculateRetirement(emp);
        
        // Apply special adjustments (accumulated over years)
        healthCost = applyHealthAdjustments(healthCost, emp);
        
        // Total cost calculation
        double totalCost = healthCost + dentalCost + lifeCost + retirementContrib;
        
        // Store results in database
        storeBenefitsCalculation(employeeId, healthCost, dentalCost, 
                               lifeCost, retirementContrib, totalCost);
        
        return new BenefitsResult(employeeId, healthCost, dentalCost, 
                                lifeCost, retirementContrib, totalCost);
    }
    
    /**
     * Health cost calculation - EXTREMELY COMPLEX
     * Contains special cases added over 15+ years
     */
    private double calculateHealthCost(EmployeeData emp) {
        double baseCost = 0;
        
        // Base cost by plan type (legacy plan codes)
        switch (emp.healthPlan) {
            case "FEHB-STD":  // Federal Employee Health Benefits Standard
                baseCost = 287.50;
                break;
            case "FEHB-HIGH": // High option plan
                baseCost = 425.75;
                break;
            case "FEHB-LOW":  // Low option plan  
                baseCost = 165.25;
                break;
            case "TRICARE":   // Military health system
                baseCost = 45.00;
                break;
            default:
                baseCost = 200.00; // Default fallback
        }
        
        // Family size multiplier (added 1995)
        if (emp.familySize > 1) {
            baseCost *= 2.5; // Magic number from legislation
        }
        
        // Grade-based adjustments (added 1998)
        if (emp.grade >= 13) {
            baseCost *= 1.15; // Senior Executive adjustment
        } else if (emp.grade <= 7) {
            baseCost *= 0.85; // Junior employee discount
        }
        
        // Years of service discount (added 2001)
        if (emp.yearsService >= 20) {
            baseCost *= 0.90; // Long service discount
        }
        
        // Apply the mysterious multiplier
        baseCost *= HEALTH_MULTIPLIER;
        
        // Special adjustment for GS-14 employees hired before 2005
        // (Congressional mandate - nobody remembers why)
        if (emp.grade == 14 && emp.hireDate.before(parseDate("2005-01-01"))) {
            baseCost += 47.83; // Exact amount specified in law
        }
        
        return Math.round(baseCost * 100.0) / 100.0; // Round to cents
    }
    
    /**
     * Dental cost - simpler but still has quirks
     */
    private double calculateDentalCost(EmployeeData emp) {
        if (!emp.hasDental) {
            return 0.0;
        }
        
        double cost = DENTAL_BASE;
        
        // Family coverage
        if (emp.familySize > 1) {
            cost += (emp.familySize - 1) * 15.75;
        }
        
        // Premium plan adjustment
        if ("PREMIUM".equals(emp.dentalPlan)) {
            cost *= 1.4;
        }
        
        return cost;
    }
    
    /**
     * Life insurance calculation
     * Based on annual salary and coverage election
     */
    private double calculateLifeInsurance(EmployeeData emp) {
        if (!emp.hasLifeInsurance) {
            return 0.0;
        }
        
        // Get annual salary from grade table
        double annualSalary = getAnnualSalary(emp.grade);
        
        // Basic coverage is 1x annual salary
        double coverage = annualSalary;
        
        // Optional coverage elections
        if (emp.lifeInsuranceMultiple > 1) {
            coverage *= emp.lifeInsuranceMultiple;
        }
        
        // Cost is $0.325 per $1000 of coverage (rate from 1987)
        double monthlyCost = (coverage / 1000.0) * 0.325;
        
        return monthlyCost;
    }
    
    /**
     * Retirement contribution calculation
     * FERS vs CSRS system (legacy complexity)
     */
    private double calculateRetirement(EmployeeData emp) {
        double contribution = 0;
        
        if ("FERS".equals(emp.retirementSystem)) {
            // Federal Employee Retirement System (newer)
            double annualSalary = getAnnualSalary(emp.grade);
            contribution = (annualSalary / 12) * 0.008; // 0.8% of monthly salary
        } else if ("CSRS".equals(emp.retirementSystem)) {
            // Civil Service Retirement System (legacy)
            double annualSalary = getAnnualSalary(emp.grade);
            contribution = (annualSalary / 12) * 0.07; // 7% of monthly salary
        }
        
        return contribution;
    }
    
    /**
     * Special health adjustments accumulated over years
     * Each if-statement represents a different policy change
     */
    private double applyHealthAdjustments(double baseCost, EmployeeData emp) {
        double adjustedCost = baseCost;
        
        // Adjustment for employees in certain zip codes (2002 policy)
        if (isHighCostArea(emp.zipCode)) {
            adjustedCost *= 1.25;
        }
        
        // Special adjustment for law enforcement (2004)
        if (isLawEnforcement(emp.jobSeries)) {
            adjustedCost *= 0.95;
        }
        
        // Temporary COVID adjustment (2020 - still in effect)
        adjustedCost += 15.00;
        
        return adjustedCost;
    }
    
    // Helper methods with hardcoded business logic
    
    private boolean isValidEmployeeId(String empId) {
        return empId != null && empId.matches("\\d{6}");
    }
    
    private double getAnnualSalary(int grade) {
        // Hardcoded GS pay scale (2008 rates - never updated)
        switch (grade) {
            case 5: return 28000;
            case 7: return 32000;
            case 9: return 38000;
            case 11: return 45000;
            case 12: return 54000;
            case 13: return 64000;
            case 14: return 76000;
            case 15: return 89000;
            default: return 35000;
        }
    }
    
    private boolean isHighCostArea(String zipCode) {
        // High cost zip codes (hardcoded list from 2002)
        String[] highCostZips = {
            "20001", "20002", "20003", "20004", "20005", // DC area
            "22101", "22102", "22103", // Northern VA
            "94101", "94102", "94103"  // San Francisco
        };
        
        for (String zip : highCostZips) {
            if (zip.equals(zipCode)) {
                return true;
            }
        }
        return false;
    }
    
    private boolean isLawEnforcement(String jobSeries) {
        // Law enforcement job series codes
        return "1811".equals(jobSeries) || "0083".equals(jobSeries);
    }
    
    private Date parseDate(String dateStr) {
        try {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
            return sdf.parse(dateStr);
        } catch (ParseException e) {
            return new Date(0); // Return epoch if parsing fails
        }
    }
    
    // Database access methods (should be refactored)
    
    private EmployeeData getEmployeeData(String empId) throws SQLException {
        employeeStmt.setString(1, empId);
        ResultSet rs = employeeStmt.executeQuery();
        
        if (rs.next()) {
            EmployeeData emp = new EmployeeData();
            emp.empId = rs.getString("emp_id");
            emp.grade = rs.getInt("grade");
            emp.yearsService = rs.getInt("years_service");
            emp.familySize = rs.getInt("family_size");
            emp.healthPlan = rs.getString("health_plan");
            emp.hasDental = "Y".equals(rs.getString("dental_plan"));
            emp.hasLifeInsurance = "Y".equals(rs.getString("life_insurance"));
            // ... more field mapping
            return emp;
        }
        
        return null;
    }
    
    private void storeBenefitsCalculation(String empId, double health, 
                                        double dental, double life, 
                                        double retirement, double total) 
                                        throws SQLException {
        benefitsStmt.setString(1, empId);
        benefitsStmt.setDouble(2, health);
        benefitsStmt.setDouble(3, dental);
        benefitsStmt.setDouble(4, life);
        benefitsStmt.setDouble(5, retirement);
        benefitsStmt.setDouble(6, total);
        benefitsStmt.executeUpdate();
    }
    
    // Inner classes for data structures
    
    private static class EmployeeData {
        String empId;
        int grade;
        int yearsService;
        int familySize;
        String healthPlan;
        String dentalPlan;
        boolean hasDental;
        boolean hasLifeInsurance;
        int lifeInsuranceMultiple;
        String retirementSystem;
        String zipCode;
        String jobSeries;
        Date hireDate;
    }
    
    public static class BenefitsResult {
        public final String employeeId;
        public final double healthCost;
        public final double dentalCost;
        public final double lifeCost;
        public final double retirementContrib;
        public final double totalCost;
        
        public BenefitsResult(String empId, double health, double dental, 
                            double life, double retirement, double total) {
            this.employeeId = empId;
            this.healthCost = health;
            this.dentalCost = dental;
            this.lifeCost = life;
            this.retirementContrib = retirement;
            this.totalCost = total;
        }
    }
    
    public static class BenefitsException extends Exception {
        public BenefitsException(String message) {
            super(message);
        }
    }
}
