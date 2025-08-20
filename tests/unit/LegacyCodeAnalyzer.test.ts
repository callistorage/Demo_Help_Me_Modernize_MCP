/**
 * Unit tests for LegacyCodeAnalyzer
 * Tests AI-powered code analysis functionality using the actual interface
 */

import { jest, describe, it, expect, beforeEach, afterEach } from '@jest/globals';
import { LegacyCodeAnalyzer } from '../../src/analyzers/LegacyCodeAnalyzer';
import { sampleFiles } from '../fixtures/testData';
import { mockCreate } from '../setup';
import { AnalysisRequest } from '../../src/types/index';

describe('LegacyCodeAnalyzer', () => {
  let analyzer: LegacyCodeAnalyzer;

  beforeEach(() => {
    analyzer = new LegacyCodeAnalyzer();
    jest.clearAllMocks();
    
    // Set up default successful mock response
    mockCreate.mockResolvedValue({
      content: [{
        type: 'text',
        text: JSON.stringify({
          summary: "COBOL payroll calculation program",
          businessLogic: ["Employee record processing", "Salary calculations"],
          complexity: "Medium",
          maintainability: "Low - Legacy COBOL structure"
        })
      }],
      usage: {
        input_tokens: 150,
        output_tokens: 200
      }
    });
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  describe('Basic Analysis Flow', () => {
    it('should analyze a single COBOL file', async () => {
      const request: AnalysisRequest = {
        files: [{
          id: 'test-123',
          name: 'payroll.cob',
          content: sampleFiles.cobol.simple,
          language: 'cobol',
          size: sampleFiles.cobol.simple.length,
          uploadedAt: new Date()
        }],
        analysisTypes: ['documentation'],
        outputFormats: ['json']
      };

      const results = await analyzer.analyzeFiles(request);

      expect(results).toHaveLength(1);
      expect(results[0].fileName).toBe('payroll.cob');
      expect(results[0].language).toBe('cobol');
      expect(results[0].analysisType).toBe('documentation');
      expect(results[0].results).toBeDefined();
    });

    it('should analyze multiple files', async () => {
      const request: AnalysisRequest = {
        files: [
          {
            id: 'test-1',
            name: 'payroll.cob',
            content: sampleFiles.cobol.simple,
            language: 'cobol',
            size: sampleFiles.cobol.simple.length,
            uploadedAt: new Date()
          },
          {
            id: 'test-2',
            name: 'Employee.java',
            content: sampleFiles.java.legacy,
            language: 'java',
            size: sampleFiles.java.legacy.length,
            uploadedAt: new Date()
          }
        ],
        analysisTypes: ['documentation'],
        outputFormats: ['json']
      };

      const results = await analyzer.analyzeFiles(request);

      expect(results).toHaveLength(2);
      expect(results[0].fileName).toBe('payroll.cob');
      expect(results[1].fileName).toBe('Employee.java');
    });

    it('should handle multiple analysis types for a single file', async () => {
      const request: AnalysisRequest = {
        files: [{
          id: 'test-123',
          name: 'test.cob',
          content: sampleFiles.cobol.simple,
          language: 'cobol',
          size: sampleFiles.cobol.simple.length,
          uploadedAt: new Date()
        }],
        analysisTypes: ['documentation', 'security-vulnerabilities'],
        outputFormats: ['json']
      };

      const results = await analyzer.analyzeFiles(request);

      expect(results).toHaveLength(2);
      expect(results[0].analysisType).toBe('documentation');
      expect(results[1].analysisType).toBe('security-vulnerabilities');
    });
  });

  describe('Language Support', () => {
    it('should analyze COBOL files', async () => {
      const request: AnalysisRequest = {
        files: [{
          id: 'cobol-1',
          name: 'legacy.cob',
          content: sampleFiles.cobol.complex,
          language: 'cobol',
          size: sampleFiles.cobol.complex.length,
          uploadedAt: new Date()
        }],
        analysisTypes: ['documentation'],
        outputFormats: ['json']
      };

      const results = await analyzer.analyzeFiles(request);

      expect(results).toHaveLength(1);
      expect(results[0].language).toBe('cobol');
      expect(mockCreate).toHaveBeenCalled();
    });

    it('should analyze Java files', async () => {
      const request: AnalysisRequest = {
        files: [{
          id: 'java-1',
          name: 'Employee.java',
          content: sampleFiles.java.legacy,
          language: 'java',
          size: sampleFiles.java.legacy.length,
          uploadedAt: new Date()
        }],
        analysisTypes: ['modernization-suggestions'],
        outputFormats: ['json']
      };

      const results = await analyzer.analyzeFiles(request);

      expect(results).toHaveLength(1);
      expect(results[0].language).toBe('java');
    });

    it('should analyze SQL files', async () => {
      const request: AnalysisRequest = {
        files: [{
          id: 'sql-1',
          name: 'queries.sql',
          content: sampleFiles.sql.legacy,
          language: 'sql',
          size: sampleFiles.sql.legacy.length,
          uploadedAt: new Date()
        }],
        analysisTypes: ['security-vulnerabilities'],
        outputFormats: ['json']
      };

      const results = await analyzer.analyzeFiles(request);

      expect(results).toHaveLength(1);
      expect(results[0].language).toBe('sql');
    });
  });

  describe('Error Handling', () => {
    it('should handle empty file list gracefully', async () => {
      const request: AnalysisRequest = {
        files: [],
        analysisTypes: ['documentation'],
        outputFormats: ['json']
      };

      const results = await analyzer.analyzeFiles(request);

      expect(results).toHaveLength(0);
    });

    it('should handle files with empty content', async () => {
      const request: AnalysisRequest = {
        files: [{
          id: 'empty-1',
          name: 'empty.cob',
          content: '',
          language: 'cobol',
          size: 0,
          uploadedAt: new Date()
        }],
        analysisTypes: ['documentation'],
        outputFormats: ['json']
      };

      const results = await analyzer.analyzeFiles(request);

      expect(results).toHaveLength(1);
      // Should handle gracefully, possibly with error result
    });

    it('should handle large files', async () => {
      const largeContent = sampleFiles.cobol.complex.repeat(100);
      const request: AnalysisRequest = {
        files: [{
          id: 'large-1',
          name: 'large.cob',
          content: largeContent,
          language: 'cobol',
          size: largeContent.length,
          uploadedAt: new Date()
        }],
        analysisTypes: ['documentation'],
        outputFormats: ['json']
      };

      const results = await analyzer.analyzeFiles(request);

      expect(results).toHaveLength(1);
      // Should complete analysis or handle size gracefully
    });
  });

  describe('Analysis Options', () => {
    it('should respect custom analysis options', async () => {
      const request: AnalysisRequest = {
        files: [{
          id: 'test-options',
          name: 'test.cob',
          content: sampleFiles.cobol.simple,
          language: 'cobol',
          size: sampleFiles.cobol.simple.length,
          uploadedAt: new Date()
        }],
        analysisTypes: ['documentation'],
        outputFormats: ['json'],
        options: {
          detailLevel: 'comprehensive',
          includeCodeExamples: true,
          maxAnalysisLines: 5000
        }
      };

      const results = await analyzer.analyzeFiles(request);

      expect(results).toHaveLength(1);
      expect(mockCreate).toHaveBeenCalled();
    });

    it('should handle missing options gracefully', async () => {
      const request: AnalysisRequest = {
        files: [{
          id: 'test-no-options',
          name: 'test.cob',
          content: sampleFiles.cobol.simple,
          language: 'cobol',
          size: sampleFiles.cobol.simple.length,
          uploadedAt: new Date()
        }],
        analysisTypes: ['documentation'],
        outputFormats: ['json']
        // No options provided
      };

      const results = await analyzer.analyzeFiles(request);

      expect(results).toHaveLength(1);
    });
  });

  describe('Result Structure', () => {
    it('should return properly structured results', async () => {
      const request: AnalysisRequest = {
        files: [{
          id: 'struct-test',
          name: 'test.cob',
          content: sampleFiles.cobol.simple,
          language: 'cobol',
          size: sampleFiles.cobol.simple.length,
          uploadedAt: new Date()
        }],
        analysisTypes: ['documentation'],
        outputFormats: ['json']
      };

      const results = await analyzer.analyzeFiles(request);

      expect(results).toHaveLength(1);
      const result = results[0];

      expect(result.id).toBeDefined();
      expect(result.fileId).toBe('struct-test');
      expect(result.fileName).toBe('test.cob');
      expect(result.language).toBe('cobol');
      expect(result.analysisType).toBe('documentation');
      expect(result.results).toBeDefined();
      expect(result.generatedAt).toBeInstanceOf(Date);
      expect(typeof result.processingTime).toBe('number');
    });

    it('should have consistent IDs and timestamps', async () => {
      const request: AnalysisRequest = {
        files: [{
          id: 'id-test',
          name: 'test.cob',
          content: sampleFiles.cobol.simple,
          language: 'cobol',
          size: sampleFiles.cobol.simple.length,
          uploadedAt: new Date()
        }],
        analysisTypes: ['documentation'],
        outputFormats: ['json']
      };

      const results = await analyzer.analyzeFiles(request);

      expect(results).toHaveLength(1);
      const result = results[0];

      expect(result.id).toBeTruthy();
      expect(result.id).toMatch(/^[a-zA-Z0-9-]+$/); // Valid ID format
      expect(result.generatedAt.getTime()).toBeLessThanOrEqual(Date.now());
    });
  });

  describe('Batch Processing', () => {
    it('should handle mixed file types in a single request', async () => {
      const request: AnalysisRequest = {
        files: [
          {
            id: 'mixed-1',
            name: 'payroll.cob',
            content: sampleFiles.cobol.simple,
            language: 'cobol',
            size: sampleFiles.cobol.simple.length,
            uploadedAt: new Date()
          },
          {
            id: 'mixed-2',
            name: 'Employee.java',
            content: sampleFiles.java.legacy,
            language: 'java',
            size: sampleFiles.java.legacy.length,
            uploadedAt: new Date()
          },
          {
            id: 'mixed-3',
            name: 'queries.sql',
            content: sampleFiles.sql.legacy,
            language: 'sql',
            size: sampleFiles.sql.legacy.length,
            uploadedAt: new Date()
          }
        ],
        analysisTypes: ['documentation'],
        outputFormats: ['json']
      };

      const results = await analyzer.analyzeFiles(request);

      expect(results).toHaveLength(3);
      expect(results.map(r => r.language)).toEqual(['cobol', 'java', 'sql']);
    });

    it('should maintain analysis type consistency across files', async () => {
      const request: AnalysisRequest = {
        files: [
          {
            id: 'batch-1',
            name: 'test1.cob',
            content: sampleFiles.cobol.simple,
            language: 'cobol',
            size: sampleFiles.cobol.simple.length,
            uploadedAt: new Date()
          },
          {
            id: 'batch-2',
            name: 'test2.java',
            content: sampleFiles.java.legacy,
            language: 'java',
            size: sampleFiles.java.legacy.length,
            uploadedAt: new Date()
          }
        ],
        analysisTypes: ['documentation', 'security-vulnerabilities'],
        outputFormats: ['json']
      };

      const results = await analyzer.analyzeFiles(request);

      expect(results).toHaveLength(4); // 2 files × 2 analysis types

      // Check that we have the right combination of results
      const analysisTypes = results.map(r => r.analysisType);
      expect(analysisTypes.filter(t => t === 'documentation')).toHaveLength(2);
      expect(analysisTypes.filter(t => t === 'security-vulnerabilities')).toHaveLength(2);
    });
  });

  describe('Performance Considerations', () => {
    it('should complete analysis within reasonable time limits', async () => {
      const startTime = Date.now();
      
      const request: AnalysisRequest = {
        files: [{
          id: 'perf-test',
          name: 'test.cob',
          content: sampleFiles.cobol.simple,
          language: 'cobol',
          size: sampleFiles.cobol.simple.length,
          uploadedAt: new Date()
        }],
        analysisTypes: ['documentation'],
        outputFormats: ['json']
      };

      const results = await analyzer.analyzeFiles(request);
      
      const elapsed = Date.now() - startTime;
      
      expect(results).toHaveLength(1);
      expect(elapsed).toBeLessThan(5000); // Should complete within 5 seconds
    });

    it('should handle concurrent analysis requests', async () => {
      const createRequest = (id: string) => ({
        files: [{
          id: `concurrent-${id}`,
          name: `test${id}.cob`,
          content: sampleFiles.cobol.simple,
          language: 'cobol' as const,
          size: sampleFiles.cobol.simple.length,
          uploadedAt: new Date()
        }],
        analysisTypes: ['documentation' as const],
        outputFormats: ['json' as const]
      });

      const requests = [
        createRequest('1'),
        createRequest('2'),
        createRequest('3')
      ];

      const results = await Promise.all(
        requests.map(req => analyzer.analyzeFiles(req))
      );

      expect(results).toHaveLength(3);
      results.forEach((resultSet, index) => {
        expect(resultSet).toHaveLength(1);
        expect(resultSet[0].fileName).toBe(`test${index + 1}.cob`);
      });
    });
  });
});
