/**
 * Integration tests for MCP Server functionality
 * Tests the complete workflow from file upload to analysis output
 */

// Ensure we use mock responses for testing
process.env.ANTHROPIC_API_KEY = '';

import { jest, describe, it, expect, beforeEach, afterEach } from '@jest/globals';
import { HelpMeModernizeServer } from '../../src/mcp/HelpMeModernizeServer';
import { FileProcessor } from '../../src/utils/FileProcessor';
import { LegacyCodeAnalyzer } from '../../src/analyzers/LegacyCodeAnalyzer';
import { OutputGenerator } from '../../src/outputs/OutputGenerator';
import { sampleFiles } from '../fixtures/testData';

describe('MCP Server Integration', () => {
  let mcpServer: HelpMeModernizeServer;
  let fileProcessor: FileProcessor;
  let analyzer: LegacyCodeAnalyzer;
  let outputGenerator: OutputGenerator;

  beforeEach(() => {
    mcpServer = new HelpMeModernizeServer();
    fileProcessor = new FileProcessor();
    analyzer = new LegacyCodeAnalyzer();
    outputGenerator = new OutputGenerator();
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  describe('End-to-End Workflow', () => {
    it('should process a complete analysis workflow', async () => {
      // Step 1: Process file
      const fileResult = await fileProcessor.processFile({
        name: 'payroll.cob',
        content: sampleFiles.cobol.simple
      });

      expect(fileResult.success).toBe(true);
      expect(fileResult.language).toBe('cobol');

      // Step 2: Analyze file  
      const codeFile = {
        id: fileResult.fileId!,
        name: fileResult.fileName,
        content: sampleFiles.cobol.simple,
        language: fileResult.language,
        size: fileResult.size,
        uploadedAt: new Date()
      };

      const analysisRequest = {
        files: [codeFile],
        analysisTypes: ['documentation' as const],
        outputFormats: ['html' as const, 'json' as const]
      };

      const analysisResults = await analyzer.analyzeFiles(analysisRequest);

      expect(analysisResults).toHaveLength(1);
      expect(analysisResults[0].fileName).toBe('payroll.cob');
      expect(analysisResults[0].language).toBe('cobol');

      // Step 3: Generate outputs
      const outputs = await outputGenerator.generateOutputs(analysisResults, ['html', 'json']);

      expect(outputs).toHaveLength(2);
      expect(outputs.find(o => o.format === 'html')).toBeDefined();
      expect(outputs.find(o => o.format === 'json')).toBeDefined();
    });

    it('should handle multiple files and analysis types', async () => {
      // Process multiple files
      const files = [
        { name: 'payroll.cob', content: sampleFiles.cobol.simple },
        { name: 'Employee.java', content: sampleFiles.java.legacy }
      ];

      const processedFiles: any[] = [];
      for (const file of files) {
        const result = await fileProcessor.processFile(file);
        expect(result.success).toBe(true);
        
        processedFiles.push({
          id: result.fileId!,
          name: result.fileName,
          content: file.content,
          language: result.language,
          size: result.size,
          uploadedAt: new Date()
        });
      }

      // Analyze with multiple types
      const analysisRequest = {
        files: processedFiles,
        analysisTypes: ['documentation' as const, 'security-vulnerabilities' as const],
        outputFormats: ['html' as const]
      };

      const analysisResults = await analyzer.analyzeFiles(analysisRequest);

      // Should have 4 results: 2 files × 2 analysis types
      expect(analysisResults).toHaveLength(4);

      // Generate comprehensive output
      const outputs = await outputGenerator.generateOutputs(analysisResults, ['html', 'json', 'markdown']);

      expect(outputs).toHaveLength(3);
      expect(outputs.every(o => o.content.length > 0)).toBe(true);
    });
  });

  describe('Error Handling Integration', () => {
    it('should handle file processing failures gracefully', async () => {
      // Try to process an invalid file
      const result = await fileProcessor.processFile({
        name: 'invalid.exe',
        content: '\x00\x01\x02\x03' // Binary content
      });

      expect(result.success).toBe(false);
      expect(result.error).toBeDefined();

      // Should not crash the workflow
      expect(result).toBeDefined();
    });

    it('should handle analysis failures without crashing', async () => {
      // Process a valid file
      const fileResult = await fileProcessor.processFile({
        name: 'test.cob',
        content: sampleFiles.cobol.simple
      });

      expect(fileResult.success).toBe(true);

      // Create file object
      const codeFile = {
        id: fileResult.fileId!,
        name: fileResult.fileName,
        content: sampleFiles.cobol.simple,
        language: fileResult.language,
        size: fileResult.size,
        uploadedAt: new Date()
      };

      // Try analysis (might fail due to API issues, but should handle gracefully)
      const analysisRequest = {
        files: [codeFile],
        analysisTypes: ['documentation' as const],
        outputFormats: ['html' as const]
      };

      const analysisResults = await analyzer.analyzeFiles(analysisRequest);

      // Should always return something, even if analysis fails
      expect(analysisResults).toBeDefined();
      expect(Array.isArray(analysisResults)).toBe(true);

      // Should be able to generate output even with partial/failed analysis
      if (analysisResults.length > 0) {
        const outputs = await outputGenerator.generateOutputs(analysisResults, ['html']);
        expect(outputs).toBeDefined();
        expect(outputs.length).toBeGreaterThan(0);
      }
    });
  });

  describe('Component Integration', () => {
    it('should maintain data consistency across components', async () => {
      const originalContent = sampleFiles.cobol.simple;
      const fileName = 'test-consistency.cob';

      // Process file
      const fileResult = await fileProcessor.processFile({
        name: fileName,
        content: originalContent
      });

      expect(fileResult.success).toBe(true);
      expect(fileResult.fileName).toBe(fileName);
      expect(fileResult.language).toBe('cobol');

      // Create analysis request
      const codeFile = {
        id: fileResult.fileId!,
        name: fileResult.fileName,
        content: originalContent,
        language: fileResult.language,
        size: fileResult.size,
        uploadedAt: new Date()
      };

      const analysisRequest = {
        files: [codeFile],
        analysisTypes: ['documentation' as const],
        outputFormats: ['json' as const]
      };

      // Analyze
      const analysisResults = await analyzer.analyzeFiles(analysisRequest);
      expect(analysisResults).toHaveLength(1);

      const result = analysisResults[0];
      expect(result.fileName).toBe(fileName);
      expect(result.language).toBe('cobol');
      expect(result.fileId).toBe(codeFile.id);

      // Generate output and verify consistency
      const outputs = await outputGenerator.generateOutputs(analysisResults, ['json']);
      expect(outputs).toHaveLength(1);

      const jsonOutput = JSON.parse(outputs[0].content);
      expect(jsonOutput.results[0].fileName).toBe(fileName);
      expect(jsonOutput.results[0].language).toBe('cobol');
    });

    it('should handle configuration consistency', async () => {
      // Test that file processor limits are respected throughout the pipeline
      const stats = fileProcessor.getProcessingStats();
      
      expect(stats.maxFileSize).toBe(50 * 1024 * 1024); // 50MB
      expect(stats.maxAnalysisLines).toBe(10000);
      expect(stats.supportedLanguages).toContain('cobol');
      expect(stats.supportedLanguages).toContain('java');
      expect(stats.supportedLanguages).toContain('sql');

      // Verify these limits are used consistently
      const largeContent = 'x'.repeat(51 * 1024 * 1024); // > 50MB
      const result = await fileProcessor.processFile({
        name: 'large.cob',
        content: largeContent
      });

      expect(result.success).toBe(false);
      expect(result.error).toBeDefined();
    });
  });

  describe('Performance Integration', () => {
    it('should handle moderate load efficiently', async () => {
      const files = Array.from({ length: 10 }, (_, i) => ({
        name: `file-${i}.cob`,
        content: sampleFiles.cobol.simple
      }));

      const startTime = Date.now();

      // Process all files
      const processedFiles: any[] = [];
      for (const file of files) {
        const result = await fileProcessor.processFile(file);
        if (result.success) {
          processedFiles.push({
            id: result.fileId!,
            name: result.fileName,
            content: file.content,
            language: result.language,
            size: result.size,
            uploadedAt: new Date()
          });
        }
      }

      // Analyze all files
      const analysisRequest = {
        files: processedFiles,
        analysisTypes: ['documentation' as const],
        outputFormats: ['html' as const]
      };

      const analysisResults = await analyzer.analyzeFiles(analysisRequest);

      // Generate outputs
      const outputs = await outputGenerator.generateOutputs(analysisResults, ['html', 'json']);

      const endTime = Date.now();
      const totalTime = endTime - startTime;

      // Should complete in reasonable time (adjust based on environment)
      expect(totalTime).toBeLessThan(60000); // 60 seconds max
      expect(outputs).toHaveLength(2);
      expect(analysisResults.length).toBeGreaterThan(0);
    });
  });

  describe('Output Format Integration', () => {
    it('should generate consistent information across all formats', async () => {
      const fileResult = await fileProcessor.processFile({
        name: 'consistency-test.cob',
        content: sampleFiles.cobol.simple
      });

      expect(fileResult.success).toBe(true);

      const codeFile = {
        id: fileResult.fileId!,
        name: fileResult.fileName,
        content: sampleFiles.cobol.simple,
        language: fileResult.language,
        size: fileResult.size,
        uploadedAt: new Date()
      };

      const analysisRequest = {
        files: [codeFile],
        analysisTypes: ['documentation' as const],
        outputFormats: ['html' as const, 'json' as const, 'markdown' as const, 'text' as const]
      };

      const analysisResults = await analyzer.analyzeFiles(analysisRequest);
      const outputs = await outputGenerator.generateOutputs(
        analysisResults, 
        ['html', 'json', 'markdown', 'text', 'pdf']
      );

      expect(outputs).toHaveLength(5);

      // All formats should contain the file name
      outputs.forEach(output => {
        if (output.format === 'json') {
          const jsonData = JSON.parse(output.content);
          expect(jsonData.results[0].fileName).toBe('consistency-test.cob');
        } else if (output.format === 'pdf') {
          // PDF is binary, just check it exists
          expect(output.content).toBeDefined();
        } else {
          expect(output.content).toContain('consistency-test.cob');
        }
      });
    });
  });
});
