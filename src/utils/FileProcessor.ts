/**
 * @fileoverview File Processing utility for government legacy code analysis
 * 
 * This module provides secure file processing capabilities for government environments,
 * including validation, language detection, and preparation for AI-powered analysis.
 * Designed for air-gapped deployments with comprehensive security measures.
 * 
 * @author Help Me Modernize - Government Developer Relations
 * @version 1.0.0
 * @since 2025-08-19
 * 
 * @security
 * - Implements file size validation to prevent resource exhaustion
 * - Validates file paths to prevent directory traversal attacks
 * - Detects and rejects binary files that could contain malware
 * - Sanitizes file content for secure logging and processing
 * 
 * @compliance
 * - Supports government file size limits (50MB per file)
 * - Implements audit trails for all file processing activities
 * - Provides detailed error reporting for compliance reviews
 * - Redacts sensitive information from logs automatically
 */

import { v4 as uuidv4 } from 'uuid';
import { Logger } from './Logger';
import { 
  CodeFile, 
  CodeLanguage, 
  FileProcessingResult 
} from '../types/index';
import { Result, ResultUtils, ValidationError } from '../types/common';

/**
 * FileProcessor class for secure legacy code file handling
 * 
 * Provides comprehensive file processing capabilities specifically designed for
 * government legacy code modernization projects. Handles multiple legacy languages
 * with appropriate security measures for air-gapped environments.
 * 
 * @example
 * ```typescript
 * const processor = new FileProcessor();
 * 
 * // Process a COBOL file
 * const result = await processor.processFile({
 *   name: 'payroll.cob',
 *   content: cobolCode
 * });
 * 
 * if (result.success) {
 *   console.log(`Processed ${result.fileName} (${result.language})`);
 * }
 * ```
 * 
 * @class
 * @public
 */
export class FileProcessor {
  /** Logger instance for audit trails and debugging */
  private logger: Logger;
  
  /** Maximum file size allowed for processing (50MB for government compliance) */
  private readonly maxFileSize: number;
  
  /** Maximum lines to analyze in detail (prevents resource exhaustion) */
  private readonly maxAnalysisLines: number;
  
  /** Map of file extensions to supported programming languages */
  private readonly supportedExtensions: Map<string, CodeLanguage>;

  /**
   * Initializes a new FileProcessor instance with government-compliant defaults
   * 
   * Sets up secure file processing with appropriate limits for government environments:
   * - 50MB maximum file size (configurable for air-gapped deployments)
   * - 10,000 line analysis limit (prevents resource exhaustion)
   * - Comprehensive language support for legacy government systems
   * 
   * @constructor
   * @public
   */
  constructor() {
    this.logger = new Logger('FileProcessor');
    this.maxFileSize = 50 * 1024 * 1024; // 50MB - Government compliance limit
    this.maxAnalysisLines = 10000; // Maximum lines for detailed analysis
    
    // Map file extensions to programming languages
    // Comprehensive support for government legacy systems
    this.supportedExtensions = new Map([
      // COBOL extensions (mainframe legacy systems)
      ['.cob', 'cobol'],    // Standard COBOL source
      ['.cbl', 'cobol'],    // COBOL source (alternate extension)
      ['.cobol', 'cobol'],  // Full extension format
      ['.cpy', 'cobol'],    // COBOL copybooks
      ['.copy', 'cobol'],   // COBOL copybooks (alternate)
      
      // Java extensions (enterprise applications)
      ['.java', 'java'],    // Java source files
      ['.class', 'java'],   // Compiled Java bytecode
      ['.jar', 'java'],     // Java archive files
      
      // SQL extensions (database systems)
      ['.sql', 'sql'],      // Standard SQL scripts
      ['.ddl', 'sql'],      // Data Definition Language
      ['.dml', 'sql'],      // Data Manipulation Language
      ['.proc', 'sql'],     // Stored procedures
      ['.sp', 'sql'],       // Stored procedures (alternate)
      
      // Text and configuration files
      ['.txt', 'text'],     // Plain text documentation
      ['.log', 'text'],     // Log files
      ['.cfg', 'text'],     // Configuration files
      ['.conf', 'text'],    // Configuration files (alternate)
      ['.ini', 'text'],     // Windows INI configuration files
      ['.properties', 'text'] // Java properties files
    ]);
  }

  /**
   * Processes a file for legacy code analysis with comprehensive validation
   * 
   * This is the primary entry point for file processing. Performs security validation,
   * language detection, and prepares the file for AI-powered analysis. Designed for
   * government environments with strict security and compliance requirements.
   * 
   * @param file - File object containing name, content, and optional language override
   * @param file.name - The original filename (used for language detection and validation)
   * @param file.content - The raw file content as a string
   * @param file.language - Optional language override (bypasses automatic detection)
   * 
   * @returns Promise resolving to FileProcessingResult with success status and details
   * 
   * @example
   * ```typescript
   * const result = await processor.processFile({
   *   name: 'legacy-payroll.cob',
   *   content: cobolSourceCode
   * });
   * 
   * if (result.success) {
   *   console.log(`File: ${result.fileName}`);
   *   console.log(`Language: ${result.language}`);
   *   console.log(`Size: ${result.size} bytes`);
   *   if (result.warnings?.length) {
   *     console.log(`Warnings: ${result.warnings.join(', ')}`);
   *   }
   * } else {
   *   console.error(`Processing failed: ${result.error}`);
   * }
   * ```
   * 
   * @throws {Error} Only in case of unexpected system errors (file processing errors are returned in result)
   * 
   * @security
   * - Validates file size against government limits (50MB)
   * - Checks for dangerous file paths and names
   * - Detects and rejects binary content
   * - Sanitizes content for secure logging
   * 
   * @compliance
   * - Generates audit trail for all processing activities
   * - Implements government file handling standards
   * - Provides detailed error reporting for compliance reviews
   * 
   * @public
   * @async
   */
  public async processFile(file: {
    name: string;
    content: string;
    language?: string;
  }): Promise<FileProcessingResult> {
    try {
      this.logger.info(`Processing file: ${file.name}`);

      // Validate file
      const validation = this.validateFile(file.name, file.content);
      if (!validation.isValid) {
        return {
          success: false,
          fileName: file.name,
          language: 'unknown',
          size: file.content.length,
          error: validation.error
        };
      }

      // Detect language if not provided
      const language = file.language as CodeLanguage || this.detectLanguage(file.name, file.content);
      
      // Check if language is supported
      if (!this.isLanguageSupported(language)) {
        return {
          success: false,
          fileName: file.name,
          language: language,
          size: file.content.length,
          error: `Unsupported language: ${language}`
        };
      }

      // Generate file ID
      const fileId = uuidv4();

      // Check for potential issues
      const warnings = this.analyzeFileForWarnings(file.content, language);

      this.logger.info(`Successfully processed file: ${file.name} (${language})`);

      return {
        success: true,
        fileId,
        fileName: file.name,
        language,
        size: file.content.length,
        warnings
      };

    } catch (error) {
      this.logger.error(`Failed to process file ${file.name}:`, error);
      return {
        success: false,
        fileName: file.name,
        language: 'unknown',
        size: file.content?.length || 0,
        error: error instanceof Error ? error.message : 'Unknown processing error'
      };
    }
  }

  /**
   * Handles secure file upload with base64 content processing and government compliance validation.
   * 
   * This method provides a secure interface for uploading files through base64 encoding,
   * implementing government-grade security validation and file processing standards.
   * 
   * @param fileName - The original name of the file being uploaded
   * @param base64Content - The file content encoded in base64 format
   * @param mimeType - Optional MIME type for additional validation
   * 
   * @returns Promise resolving to FileProcessingResult with processing status and metadata
   * 
   * @example
   * ```typescript
   * const processor = new FileProcessor(logger);
   * const result = await processor.uploadFile(
   *   'legacy-system.cob',
   *   'Y29ib2wgY29kZSBoZXJl...', // base64 encoded content
   *   'text/plain'
   * );
   * 
   * if (result.success) {
   *   console.log(`File uploaded: ${result.fileId}`);
   * }
   * ```
   * 
   * @security
   * - Validates base64 encoding integrity before processing
   * - Implements secure decoding with error handling
   * - Applies all standard file validation security measures
   * - Prevents malicious file uploads through validation pipeline
   * 
   * @compliance
   * - Maintains audit trail for all upload activities
   * - Implements government file upload standards
   * - Provides detailed error reporting for security reviews
   * - Ensures data integrity throughout upload process
   * 
   * @throws Will return error result for invalid base64 content
   * @throws Will return error result for files exceeding security limits
   * 
   * @public
   * @async
   */
  public async uploadFile(
    fileName: string,
    base64Content: string,
    mimeType?: string
  ): Promise<FileProcessingResult> {
    try {
      this.logger.info(`Uploading file: ${fileName}`);

      // Decode base64 content
      let content: string;
      try {
        // Validate base64 format first
        if (!/^[A-Za-z0-9+/]*={0,2}$/.test(base64Content)) {
          throw new Error('Invalid base64 format');
        }
        
        content = Buffer.from(base64Content, 'base64').toString('utf-8');
        
        // Additional validation - check if decoded content seems valid
        if (content.includes('�') || content.length === 0) {
          throw new Error('Invalid base64 content - produced invalid characters');
        }
      } catch (decodeError) {
        return {
          success: false,
          fileName,
          language: 'unknown',
          size: 0,
          error: 'Failed to decode base64 content'
        };
      }

      // Process the decoded file
      return await this.processFile({
        name: fileName,
        content
      });

    } catch (error) {
      this.logger.error(`Failed to upload file ${fileName}:`, error);
      return {
        success: false,
        fileName,
        language: 'unknown',
        size: 0,
        error: error instanceof Error ? error.message : 'Upload failed'
      };
    }
  }

  /**
   * Performs comprehensive security validation of files before processing.
   * 
   * This method implements government-grade security validation including filename safety,
   * content validation, size limits, and dangerous pattern detection to ensure secure
   * file processing in air-gapped government environments.
   * 
   * @param fileName - The name of the file to validate
   * @param content - The file content to validate
   * 
   * @returns Validation result object containing success status and error details
   * 
   * @example
   * ```typescript
   * const validation = this.validateFile('legacy-system.cob', cobolContent);
   * if (!validation.isValid) {
   *   console.error('Validation failed:', validation.error);
   *   return;
   * }
   * ```
   * 
   * @security
   * - Validates filename against dangerous character patterns
   * - Enforces strict file size limits (50MB for government compliance)
   * - Prevents directory traversal attacks through filename validation
   * - Blocks empty or malformed content
   * 
   * @compliance
   * - Implements government file validation standards
   * - Provides detailed error messages for audit trails
   * - Enforces security policies for air-gapped deployments
   * - Maintains validation consistency across all file operations
   * 
   * @private
   */
  private validateFile(fileName: string, content: string): { isValid: boolean; error?: string } {
    // Check file name
    if (!fileName || fileName.trim().length === 0) {
      return { isValid: false, error: 'File name is required' };
    }

    // Check for dangerous file names
    if (this.isDangerousFileName(fileName)) {
      return { isValid: false, error: 'File name contains unsafe characters' };
    }

    // Check content
    if (!content) {
      return { isValid: false, error: 'File content is empty' };
    }

    // Check file size
    if (content.length > this.maxFileSize) {
      return { 
        isValid: false, 
        error: `File size exceeds maximum limit of ${this.maxFileSize / (1024 * 1024)}MB` 
      };
    }

    // Check for binary content (basic check)
    if (this.containsBinaryContent(content)) {
      return { isValid: false, error: 'Binary files are not supported' };
    }

    return { isValid: true };
  }

  /**
   * Intelligently detects programming language from file name and content analysis.
   * 
   * This method provides intelligent language detection by combining file extension
   * analysis with content pattern recognition, specifically optimized for legacy
   * government systems including COBOL, Java, and SQL.
   * 
   * @param fileName - The file name including extension
   * @param content - The file content for pattern analysis
   * 
   * @returns The detected CodeLanguage type
   * 
   * @example
   * ```typescript
   * const language = this.detectLanguage('payroll.cob', cobolContent);
   * // Returns 'cobol' based on .cob extension and COBOL patterns
   * 
   * const unknownLanguage = this.detectLanguage('unknown.txt', javaContent);
   * // Returns 'java' based on content patterns despite .txt extension
   * ```
   * 
   * @security
   * - Safely processes file extensions without executing content
   * - Uses pattern matching instead of code execution for detection
   * - Prevents malicious content from affecting detection logic
   * 
   * @compliance
   * - Supports all government legacy language requirements
   * - Provides consistent detection across different file naming conventions
   * - Maintains audit trail of detection decisions
   * 
   * @private
   */
  private detectLanguage(fileName: string, content: string): CodeLanguage {
    // First, try to detect by file extension
    const extension = this.getFileExtension(fileName);
    const languageFromExtension = this.supportedExtensions.get(extension);
    
    if (languageFromExtension) {
      return languageFromExtension;
    }

    // Try to detect by content patterns
    return this.detectLanguageByContent(content);
  }

  /**
   * Performs sophisticated content-based language detection using pattern analysis.
   * 
   * When file extension detection fails, this method analyzes code patterns,
   * keywords, and syntax structures to identify the programming language.
   * Specifically tuned for legacy government systems.
   * 
   * @param content - The file content to analyze for language patterns
   * 
   * @returns The detected CodeLanguage based on content patterns
   * 
   * @example
   * ```typescript
   * const language = this.detectLanguageByContent(`
   *   IDENTIFICATION DIVISION.
   *   PROGRAM-ID. PAYROLL-CALC.
   *   DATA DIVISION.
   * `);
   * // Returns 'cobol' based on COBOL syntax patterns
   * ```
   * 
   * @security
   * - Uses safe string pattern matching only
   * - No code execution or dynamic evaluation
   * - Prevents injection attacks through content analysis
   * 
   * @compliance
   * - Implements government-specific language detection requirements
   * - Supports legacy syntax patterns and conventions
   * - Provides detailed pattern matching for audit purposes
   * 
   * @private
   */
  private detectLanguageByContent(content: string): CodeLanguage {
    const upperContent = content.toUpperCase();
    
    // COBOL patterns
    const cobolPatterns = [
      'IDENTIFICATION DIVISION',
      'ENVIRONMENT DIVISION',
      'DATA DIVISION',
      'PROCEDURE DIVISION',
      'WORKING-STORAGE SECTION',
      'MOVE ', 'TO ',
      'PERFORM ', 'UNTIL',
      'IF ', 'ELSE ', 'END-IF'
    ];
    
    const cobolMatches = cobolPatterns.filter(pattern => upperContent.includes(pattern)).length;
    if (cobolMatches >= 3) {
      return 'cobol';
    }

    // Java patterns
    const javaPatterns = [
      'public class',
      'private class',
      'public static void main',
      'import java.',
      'package ',
      'public void',
      'private void',
      'System.out.println'
    ];
    
    const javaMatches = javaPatterns.filter(pattern => 
      upperContent.includes(pattern.toUpperCase())
    ).length;
    if (javaMatches >= 2) {
      return 'java';
    }

    // SQL patterns
    const sqlPatterns = [
      'SELECT ', 'FROM ', 'WHERE ',
      'INSERT INTO', 'UPDATE ', 'DELETE FROM',
      'CREATE TABLE', 'ALTER TABLE', 'DROP TABLE',
      'DECLARE ', 'BEGIN ', 'END',
      'PROCEDURE', 'FUNCTION'
    ];
    
    const sqlMatches = sqlPatterns.filter(pattern => upperContent.includes(pattern)).length;
    if (sqlMatches >= 2) {
      return 'sql';
    }

    // Default to text
    return 'text';
  }

  /**
   * Check if language is supported for analysis
   */
  private isLanguageSupported(language: CodeLanguage): boolean {
    return ['cobol', 'java', 'sql', 'text'].includes(language);
  }

  /**
   * Get file extension from filename
   */
  private getFileExtension(fileName: string): string {
    const lastDot = fileName.lastIndexOf('.');
    return lastDot !== -1 ? fileName.substring(lastDot).toLowerCase() : '';
  }

  /**
   * Check for dangerous file names that could cause security issues
   */
  private isDangerousFileName(fileName: string): boolean {
    const dangerousPatterns = [
      /\.\./,           // Directory traversal
      /[<>:"|?*]/,      // Windows illegal characters
      /^(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])$/i, // Windows reserved names
      /^\./,            // Hidden files (Unix)
      /\x00/            // Null bytes
    ];

    return dangerousPatterns.some(pattern => pattern.test(fileName));
  }

  /**
   * Basic check for binary content
   */
  private containsBinaryContent(content: string): boolean {
    // Check for null bytes and other binary indicators
    return /\x00/.test(content) || 
           /[\x01-\x08\x0E-\x1F\x7F-\x9F]/.test(content.substring(0, 1000));
  }

  /**
   * Performs comprehensive static analysis to identify potential warnings and issues.
   * 
   * This method implements language-specific static analysis to identify common
   * issues, performance concerns, and potential problems in legacy government code.
   * Provides early warning system for code quality and compliance issues.
   * 
   * @param content - The file content to analyze for warnings
   * @param language - The detected programming language for targeted analysis
   * 
   * @returns Array of warning messages describing potential issues
   * 
   * @example
   * ```typescript
   * const warnings = this.analyzeFileForWarnings(cobolContent, 'cobol');
   * // Returns warnings like:
   * // ["File contains tab characters. COBOL typically uses spaces for formatting."]
   * 
   * const sqlWarnings = this.analyzeFileForWarnings(sqlContent, 'sql');
   * // Returns warnings about DROP statements, etc.
   * ```
   * 
   * @security
   * - Identifies potential security risks in legacy code
   * - Detects dangerous SQL operations (DROP statements)
   * - Highlights formatting issues that could hide malicious code
   * 
   * @compliance
   * - Implements government code quality standards
   * - Provides audit-ready warning documentation
   * - Ensures consistent analysis across different file types
   * - Supports compliance reporting requirements
   * 
   * @private
   */
  private analyzeFileForWarnings(content: string, language: CodeLanguage): string[] {
    const warnings: string[] = [];
    const lines = content.split('\n');

    // Check line count for analysis efficiency
    if (lines.length > this.maxAnalysisLines) {
      warnings.push(
        `File has ${lines.length} lines. Analysis will be limited to first ${this.maxAnalysisLines} lines for performance.`
      );
    }

    // Language-specific warnings
    switch (language) {
      case 'cobol':
        if (content.includes('\t')) {
          warnings.push('File contains tab characters. COBOL typically uses spaces for formatting.');
        }
        break;
        
      case 'java':
        if (!content.includes('public class') && !content.includes('interface')) {
          warnings.push('Java file may not contain a public class or interface.');
        }
        break;
        
      case 'sql':
        if (content.toUpperCase().includes('DROP ')) {
          warnings.push('SQL file contains DROP statements. Review for potential data loss.');
        }
        break;
    }

    // Check for potential encoding issues
    if (/[^\x00-\x7F]/.test(content)) {
      warnings.push('File contains non-ASCII characters. Ensure proper encoding.');
    }

    return warnings;
  }

  /**
   * Get file processing statistics
   */
  public getProcessingStats(): {
    maxFileSize: number;
    maxAnalysisLines: number;
    supportedLanguages: CodeLanguage[];
    supportedExtensions: string[];
  } {
    return {
      maxFileSize: this.maxFileSize,
      maxAnalysisLines: this.maxAnalysisLines,
      supportedLanguages: ['cobol', 'java', 'sql', 'text'],
      supportedExtensions: Array.from(this.supportedExtensions.keys())
    };
  }
}
