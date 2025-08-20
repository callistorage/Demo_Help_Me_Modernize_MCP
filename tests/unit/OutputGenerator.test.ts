/**
 * Unit tests for OutputGenerator
 * Tests multi-format output generation for analysis results
 */

import { jest, describe, it, expect, beforeEach } from '@jest/globals';
import { OutputGenerator } from '../../src/outputs/OutputGenerator';
import { sampleFiles } from '../fixtures/testData';
import { AnalysisResult, OutputFormat } from '../../src/types/index';

describe('OutputGenerator', () => {
  let outputGenerator: OutputGenerator;

  beforeEach(() => {
    outputGenerator = new OutputGenerator();
  });

  const createMockAnalysisResult = (): AnalysisResult => ({
    id: 'test-analysis-123',
    fileId: 'test-file-456',
    fileName: 'payroll.cob',
    language: 'cobol',
    analysisType: 'documentation',
    results: {
      summary: 'COBOL payroll calculation program with employee record processing',
      findings: [
        {
          id: 'finding-1',
          type: 'issue',
          severity: 'medium',
          title: 'Deprecated COBOL syntax',
          description: 'Uses older COBOL patterns that could be modernized',
          location: {
            startLine: 10,
            endLine: 15,
            startColumn: 1,
            endColumn: 20
          },
          impact: 'maintenance'
        }
      ],
      recommendations: [
        {
          id: 'rec-1',
          type: 'modernization',
          priority: 'high',
          title: 'Update to modern COBOL syntax',
          description: 'Replace deprecated constructs with modern equivalents',
          effort: 'medium',
          benefits: ['Improved maintainability', 'Better readability']
        }
      ],
      metadata: {
        linesAnalyzed: 150,
        functionsFound: 5,
        complexity: 12,
        maintainabilityIndex: 65,
        estimatedAge: '15+ years',
        patterns: ['file-processing', 'batch-calculation']
      }
    },
    generatedAt: new Date(),
    processingTime: 1500
  });

  describe('Multi-format Output Generation', () => {
    it('should generate multiple output formats', async () => {
      const analysisResult = createMockAnalysisResult();
      const formats: OutputFormat[] = ['html', 'markdown', 'json'];
      
      const outputs = await outputGenerator.generateOutputs([analysisResult], formats);

      expect(outputs).toHaveLength(3);
      
      const htmlOutput = outputs.find(o => o.format === 'html');
      const markdownOutput = outputs.find(o => o.format === 'markdown');
      const jsonOutput = outputs.find(o => o.format === 'json');

      expect(htmlOutput).toBeDefined();
      expect(markdownOutput).toBeDefined();
      expect(jsonOutput).toBeDefined();

      expect(htmlOutput!.content).toContain('<!DOCTYPE html>');
      expect(htmlOutput!.mimeType).toBe('text/html');
      
      expect(markdownOutput!.content).toContain('# 🏛️ Legacy Code Analysis Report');
      expect(markdownOutput!.mimeType).toBe('text/markdown');
      
      expect(() => JSON.parse(jsonOutput!.content)).not.toThrow();
      expect(jsonOutput!.mimeType).toBe('application/json');
    });

    it('should generate single format outputs', async () => {
      const analysisResult = createMockAnalysisResult();
      
      const outputs = await outputGenerator.generateOutputs([analysisResult], ['html']);

      expect(outputs).toHaveLength(1);
      expect(outputs[0].format).toBe('html');
      expect(outputs[0].content).toContain('payroll.cob');
      expect(outputs[0].content).toContain('COBOL payroll calculation');
    });

    it('should include all analysis data in outputs', async () => {
      const analysisResult = createMockAnalysisResult();
      
      const outputs = await outputGenerator.generateOutputs([analysisResult], ['html', 'json']);

      const htmlOutput = outputs.find(o => o.format === 'html')!;
      const jsonOutput = outputs.find(o => o.format === 'json')!;

      // Check HTML contains key information
      expect(htmlOutput.content).toContain('Deprecated COBOL syntax');
      expect(htmlOutput.content).toContain('Update to modern COBOL syntax');
      expect(htmlOutput.content).toContain('payroll.cob');

      // Check JSON structure
      const jsonData = JSON.parse(jsonOutput.content);
      expect(jsonData.results).toHaveLength(1);
      expect(jsonData.results[0].fileName).toBe('payroll.cob');
      expect(jsonData.results[0].results.findings).toHaveLength(1);
      expect(jsonData.results[0].results.recommendations).toHaveLength(1);
    });
  });

  describe('HTML Output', () => {
    it('should generate valid HTML structure', async () => {
      const analysisResult = createMockAnalysisResult();
      
      const outputs = await outputGenerator.generateOutputs([analysisResult], ['html']);
      const htmlOutput = outputs[0].content;

      expect(htmlOutput).toContain('<!DOCTYPE html>');
      expect(htmlOutput).toContain('<html');
      expect(htmlOutput).toContain('<head>');
      expect(htmlOutput).toContain('<body>');
      expect(htmlOutput).toContain('</html>');
    });

    it('should include styling and be self-contained', async () => {
      const analysisResult = createMockAnalysisResult();
      
      const outputs = await outputGenerator.generateOutputs([analysisResult], ['html']);
      const htmlOutput = outputs[0].content;

      expect(htmlOutput).toContain('<style>');
      expect(htmlOutput).toContain('font-family');
      expect(htmlOutput).toContain('color');
    });

    it('should properly escape HTML content', async () => {
      const analysisResult = createMockAnalysisResult();
      analysisResult.results.findings[0].description = 'Uses <script>alert("xss")</script> patterns';
      
      const outputs = await outputGenerator.generateOutputs([analysisResult], ['html']);
      const htmlOutput = outputs[0].content;

      expect(htmlOutput).not.toContain('<script>alert("xss")</script>');
      expect(htmlOutput).toContain('&lt;script&gt;');
    });
  });

  describe('Markdown Output', () => {
    it('should generate valid Markdown structure', async () => {
      const analysisResult = createMockAnalysisResult();
      
      const outputs = await outputGenerator.generateOutputs([analysisResult], ['markdown']);
      const markdownOutput = outputs[0].content;

      expect(markdownOutput).toContain('# 🏛️ Legacy Code Analysis Report');
      expect(markdownOutput).toContain('## 📄 payroll.cob');
      expect(markdownOutput).toContain('### 📊 Documentation');
      expect(markdownOutput).toContain('(COBOL)');
    });

    it('should format findings and recommendations properly', async () => {
      const analysisResult = createMockAnalysisResult();
      
      const outputs = await outputGenerator.generateOutputs([analysisResult], ['markdown']);
      const markdownOutput = outputs[0].content;

      expect(markdownOutput).toMatch(/####\s+🔍 Findings/);
      expect(markdownOutput).toMatch(/####\s+💡 Recommendations/);
      expect(markdownOutput).toContain('- **Deprecated COBOL syntax**');
      expect(markdownOutput).toContain('- **Update to modern COBOL syntax**');
    });
  });

  describe('JSON Output', () => {
    it('should generate valid JSON with proper structure', async () => {
      const analysisResult = createMockAnalysisResult();
      
      const outputs = await outputGenerator.generateOutputs([analysisResult], ['json']);
      const jsonOutput = outputs[0].content;

      expect(() => JSON.parse(jsonOutput)).not.toThrow();
      
      const parsed = JSON.parse(jsonOutput);
      expect(parsed.metadata).toBeDefined();
      expect(parsed.results).toHaveLength(1);
      expect(parsed.generatedAt).toBeDefined();
    });

    it('should include comprehensive metadata', async () => {
      const analysisResult = createMockAnalysisResult();
      
      const outputs = await outputGenerator.generateOutputs([analysisResult], ['json']);
      const jsonData = JSON.parse(outputs[0].content);

      expect(jsonData.metadata.totalFiles).toBe(1);
      expect(jsonData.metadata.totalFindings).toBe(1);
      expect(jsonData.metadata.totalRecommendations).toBe(1);
      expect(jsonData.metadata.languages).toContain('cobol');
      expect(jsonData.metadata.analysisTypes).toContain('documentation');
    });
  });

  describe('Text Output', () => {
    it('should generate readable plain text', async () => {
      const analysisResult = createMockAnalysisResult();
      
      const outputs = await outputGenerator.generateOutputs([analysisResult], ['text']);
      const textOutput = outputs[0].content;

      expect(textOutput).toContain('LEGACY CODE ANALYSIS REPORT');
      expect(textOutput).toContain('FILE: payroll.cob');
      expect(textOutput).toContain('(COBOL)');
      expect(textOutput).toContain('Deprecated COBOL syntax');
      
      // Should not contain HTML or Markdown formatting
      expect(textOutput).not.toContain('<');
      expect(textOutput).not.toContain('#');
      expect(textOutput).not.toContain('**');
    });

    it('should use consistent formatting with separators', async () => {
      const analysisResult = createMockAnalysisResult();
      
      const outputs = await outputGenerator.generateOutputs([analysisResult], ['text']);
      const textOutput = outputs[0].content;

      expect(textOutput).toMatch(/={10,}/); // Section separators
      expect(textOutput).toMatch(/-{5,}/); // Subsection separators
    });
  });

  describe('PDF Output', () => {
    it('should generate valid PDF output', async () => {
      const analysisResult = createMockAnalysisResult();
      
      const outputs = await outputGenerator.generateOutputs([analysisResult], ['pdf']);
      const pdfOutput = outputs[0];

      expect(pdfOutput.format).toBe('pdf');
      expect(pdfOutput.mimeType).toBe('application/pdf');
      
      // Content should be base64 encoded PDF data
      expect(pdfOutput.content).toBeDefined();
      expect(pdfOutput.content.length).toBeGreaterThan(0);
    });

    it('should have proper PDF filename', async () => {
      const analysisResult = createMockAnalysisResult();
      
      const outputs = await outputGenerator.generateOutputs([analysisResult], ['pdf']);
      const pdfOutput = outputs[0];

      expect(pdfOutput.filename).toMatch(/\.pdf$/);
      expect(pdfOutput.filename).toContain('analysis');
    });
  });

  describe('Government Report Generation', () => {
    it('should generate government-specific reports', async () => {
      const analysisResult = createMockAnalysisResult();
      
      const report = await outputGenerator.generateGovernmentReport(
        [analysisResult], 
        ['FISMA', 'NIST'], 
        'internal'
      );

      expect(report.executiveSummary).toBeDefined();
      expect(report.technicalFindings).toHaveLength(1);
      expect(report.riskAssessment).toBeDefined();
      expect(report.recommendedActions).toBeDefined();
      expect(report.confidentialityLevel).toBe('internal');
      expect(report.generatedAt).toBeInstanceOf(Date);
    });

    it('should include risk assessment', async () => {
      const analysisResult = createMockAnalysisResult();
      
      const report = await outputGenerator.generateGovernmentReport(
        [analysisResult], 
        ['FISMA'], 
        'confidential'
      );

      expect(report.riskAssessment.overallRisk).toBeDefined();
      expect(report.riskAssessment.securityRisk).toBeDefined();
      expect(report.riskAssessment.maintenanceRisk).toBeDefined();
      expect(report.riskAssessment.businessRisk).toBeDefined();
      expect(Array.isArray(report.riskAssessment.mitigationStrategies)).toBe(true);
    });

    it('should generate appropriate recommended actions', async () => {
      const analysisResult = createMockAnalysisResult();
      
      const report = await outputGenerator.generateGovernmentReport(
        [analysisResult], 
        ['NIST', 'FedRAMP'], 
        'public'
      );

      expect(report.recommendedActions.length).toBeGreaterThan(0);
      
      const action = report.recommendedActions[0];
      expect(action.priority).toBeDefined();
      expect(action.category).toBeDefined();
      expect(action.action).toBeDefined();
      expect(action.timeline).toBeDefined();
      expect(action.effort).toBeDefined();
      expect(action.impact).toBeDefined();
    });

    it('should include compliance checks', async () => {
      const analysisResult = createMockAnalysisResult();
      
      const report = await outputGenerator.generateGovernmentReport(
        [analysisResult], 
        ['FISMA', 'NIST', 'FedRAMP'], 
        'internal'
      );

      expect(report.complianceChecks).toBeDefined();
      expect(report.complianceChecks.length).toBe(3); // One for each standard
      
      const fimaCheck = report.complianceChecks.find(c => c.standard === 'FISMA');
      expect(fimaCheck).toBeDefined();
      expect(typeof fimaCheck!.passed).toBe('boolean');
      expect(Array.isArray(fimaCheck!.findings)).toBe(true);
      expect(Array.isArray(fimaCheck!.recommendations)).toBe(true);
    });
  });

  describe('Error Handling', () => {
    it('should handle empty results gracefully', async () => {
      const outputs = await outputGenerator.generateOutputs([], ['html', 'json']);

      expect(outputs).toHaveLength(2);
      
      const htmlOutput = outputs.find(o => o.format === 'html')!;
      const jsonOutput = outputs.find(o => o.format === 'json')!;

      expect(htmlOutput.content).toContain('No analysis results available');
      
      const jsonData = JSON.parse(jsonOutput.content);
      expect(jsonData.results).toHaveLength(0);
    });

    it('should continue with other formats if one fails', async () => {
      const analysisResult = createMockAnalysisResult();
      
      // This should not throw even if individual formats might have issues
      const outputs = await outputGenerator.generateOutputs([analysisResult], ['html', 'json', 'markdown']);

      // Should generate outputs for successful formats
      expect(outputs.length).toBeGreaterThan(0);
    });

    it('should handle malformed analysis results', async () => {
      const malformedResult = {
        id: 'test',
        fileName: 'test.cob',
        language: 'cobol',
        analysisType: 'documentation',
        results: {
          summary: 'Test',
          findings: [],
          recommendations: [],
          metadata: {
            linesAnalyzed: 0,
            functionsFound: 0,
            complexity: 0,
            patterns: []
          }
        },
        generatedAt: new Date(),
        processingTime: 0
      } as any;

      const outputs = await outputGenerator.generateOutputs([malformedResult], ['html']);
      
      expect(outputs).toHaveLength(1);
      expect(outputs[0].content).toBeDefined();
    });
  });

  describe('Multiple Analysis Results', () => {
    it('should handle multiple files and analysis types', async () => {
      const results = [
        createMockAnalysisResult(),
        {
          ...createMockAnalysisResult(),
          id: 'test-analysis-456',
          fileId: 'test-file-789',
          fileName: 'Employee.java',
          language: 'java' as const,
          analysisType: 'security-vulnerabilities' as const
        }
      ];

      const outputs = await outputGenerator.generateOutputs(results, ['html', 'json']);

      expect(outputs).toHaveLength(2);
      
      const htmlOutput = outputs.find(o => o.format === 'html')!;
      expect(htmlOutput.content).toContain('payroll.cob');
      expect(htmlOutput.content).toContain('Employee.java');

      const jsonData = JSON.parse(outputs.find(o => o.format === 'json')!.content);
      expect(jsonData.results).toHaveLength(2);
      expect(jsonData.metadata.totalFiles).toBe(2);
    });

    it('should aggregate statistics correctly', async () => {
      const results = Array.from({ length: 5 }, (_, i) => ({
        ...createMockAnalysisResult(),
        id: `analysis-${i}`,
        fileId: `file-id-${i}`,
        fileName: `file-${i}.cob`
      }));

      const outputs = await outputGenerator.generateOutputs(results, ['json']);
      const jsonData = JSON.parse(outputs[0].content);

      expect(jsonData.metadata.totalFiles).toBe(5);
      expect(jsonData.metadata.totalFindings).toBe(5); // 1 finding per result
      expect(jsonData.metadata.totalRecommendations).toBe(5); // 1 recommendation per result
      expect(jsonData.results).toHaveLength(5);
    });
  });

  describe('Performance', () => {
    it('should handle large result sets efficiently', async () => {
      const results = Array.from({ length: 50 }, (_, i) => ({
        ...createMockAnalysisResult(),
        id: `analysis-${i}`,
        fileId: `file-id-${i}`,
        fileName: `file-${i}.cob`
      }));

      const startTime = Date.now();
      const outputs = await outputGenerator.generateOutputs(results, ['html', 'json']);
      const endTime = Date.now();

      expect(outputs).toHaveLength(2);
      expect(endTime - startTime).toBeLessThan(10000); // Should complete in under 10 seconds
    });
  });
});
