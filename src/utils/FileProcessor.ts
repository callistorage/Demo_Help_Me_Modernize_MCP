/**
 * File Processing utility for legacy code analysis
 * Handles file upload, validation, and preparation for analysis
 */

import { v4 as uuidv4 } from 'uuid';
import { Logger } from './Logger';
import { 
  CodeFile, 
  CodeLanguage, 
  FileProcessingResult 
} from '../types/index';

export class FileProcessor {
  private logger: Logger;
  private readonly maxFileSize: number;
  private readonly maxAnalysisLines: number;
  private readonly supportedExtensions: Map<string, CodeLanguage>;

  constructor() {
    this.logger = new Logger('FileProcessor');
    this.maxFileSize = 50 * 1024 * 1024; // 50MB
    this.maxAnalysisLines = 10000; // Maximum lines for detailed analysis
    
    // Map file extensions to programming languages
    this.supportedExtensions = new Map([
      // COBOL extensions
      ['.cob', 'cobol'],
      ['.cbl', 'cobol'],
      ['.cobol', 'cobol'],
      ['.cpy', 'cobol'],
      ['.copy', 'cobol'],
      
      // Java extensions
      ['.java', 'java'],
      ['.class', 'java'],
      ['.jar', 'java'],
      
      // SQL extensions
      ['.sql', 'sql'],
      ['.ddl', 'sql'],
      ['.dml', 'sql'],
      ['.proc', 'sql'],
      ['.sp', 'sql'],
      
      // Text and documentation
      ['.txt', 'text'],
      ['.log', 'text'],
      ['.cfg', 'text'],
      ['.conf', 'text'],
      ['.ini', 'text'],
      ['.properties', 'text']
    ]);
  }

  /**
   * Process a file for analysis
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
   * Handle file upload with base64 content
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
        content = Buffer.from(base64Content, 'base64').toString('utf-8');
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
   * Validate file for processing
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
   * Detect programming language from file name and content
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
   * Detect language by analyzing content patterns
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
   * Analyze file for potential warnings
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
