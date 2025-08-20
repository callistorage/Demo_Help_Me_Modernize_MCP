/**
 * @fileoverview Enterprise Error Handling System
 * 
 * This module provides comprehensive error handling capabilities with structured
 * error types, logging integration, and government compliance features.
 * 
 * @module ErrorHandler
 * @version 1.0.0
 * @author Help Me Modernize MCP Server
 * @since 2024
 * 
 * @security
 * - Sanitizes error messages to prevent information leakage
 * - Implements secure error logging with audit trails
 * - Prevents stack trace exposure in production environments
 * - Provides structured error responses for security analysis
 * 
 * @compliance
 * - Generates audit-compliant error reports
 * - Implements government error handling standards
 * - Provides detailed error tracking for compliance reviews
 * - Supports regulatory reporting requirements
 */

import { v4 as uuidv4 } from 'uuid';
import { Logger } from './Logger';
import { AppError, Result, ResultUtils } from '../types/common';

/**
 * Error severity levels for government compliance.
 */
export type ErrorSeverity = 'LOW' | 'MEDIUM' | 'HIGH' | 'CRITICAL';

/**
 * Error types for categorization and handling.
 */
export type ErrorType = 'VALIDATION' | 'BUSINESS' | 'SYSTEM' | 'SECURITY' | 'COMPLIANCE';

/**
 * Error context interface for debugging and audit trails.
 */
export interface ErrorContext {
  /** User ID if available */
  readonly userId?: string;
  /** Request ID for tracing */
  readonly requestId?: string;
  /** Operation being performed */
  readonly operation?: string;
  /** Field that caused the error */
  readonly field?: string;
  /** Additional context data */
  readonly data?: Record<string, unknown>;
  /** Timestamp when error occurred */
  readonly timestamp: Date;
}

/**
 * Enterprise error handler for government systems.
 * 
 * Provides comprehensive error handling with structured error types,
 * security-focused error sanitization, and audit-compliant logging.
 * 
 * @example
 * ```typescript
 * const errorHandler = new ErrorHandler();
 * 
 * // Handle a validation error
 * const error = errorHandler.createError({
 *   type: 'VALIDATION',
 *   severity: 'MEDIUM',
 *   message: 'Invalid file format',
 *   details: 'File extension .exe is not allowed',
 *   context: { fileName: 'malicious.exe', operation: 'file-upload' }
 * });
 * 
 * // Convert to safe response
 * const response = errorHandler.toSafeResponse(error);
 * ```
 * 
 * @security
 * - Sanitizes all error messages for production use
 * - Prevents sensitive information exposure
 * - Implements secure logging practices
 * - Provides audit trails for security analysis
 * 
 * @compliance
 * - Government-grade error handling standards
 * - Audit-compliant error logging and reporting
 * - Detailed error categorization for compliance
 * - Regulatory reporting support
 * 
 * @public
 */
export class ErrorHandler {
  private readonly logger: Logger;
  private readonly isProduction: boolean;

  /**
   * Initializes the error handler with logging and environment detection.
   * 
   * @param logger - Optional logger instance (creates new one if not provided)
   */
  constructor(logger?: Logger) {
    this.logger = logger || new Logger('ErrorHandler');
    this.isProduction = process.env.NODE_ENV === 'production' || 
                       process.env.AIR_GAPPED === 'true';
  }

  /**
   * Creates a structured application error.
   * 
   * @param params - Error creation parameters
   * @returns Structured AppError instance
   */
  public createError(params: {
    type: ErrorType;
    severity: ErrorSeverity;
    message: string;
    details?: string;
    context?: Partial<ErrorContext>;
    cause?: Error;
  }): AppError {
    const error: AppError = {
      id: uuidv4(),
      type: params.type,
      severity: params.severity,
      message: params.message,
      details: params.details,
      timestamp: new Date(),
      context: {
        timestamp: new Date(),
        ...params.context
      },
      cause: params.cause
    };

    // Log the error
    this.logError(error);

    return error;
  }

  /**
   * Handles unexpected errors and converts them to structured errors.
   * 
   * @param error - The unexpected error to handle
   * @param context - Additional context for the error
   * @returns Structured AppError instance
   */
  public handleUnexpectedError(
    error: unknown,
    context?: Partial<ErrorContext>
  ): AppError {
    const message = error instanceof Error ? error.message : 'Unknown error occurred';
    const cause = error instanceof Error ? error : undefined;

    return this.createError({
      type: 'SYSTEM',
      severity: 'HIGH',
      message: 'Unexpected system error',
      details: this.isProduction ? undefined : message,
      context,
      cause
    });
  }

  /**
   * Converts an AppError to a safe response for external consumption.
   * 
   * @param error - The error to convert
   * @returns Safe error response object
   */
  public toSafeResponse(error: AppError): {
    success: false;
    error: {
      id: string;
      type: string;
      severity: string;
      message: string;
      timestamp: string;
    };
  } {
    return {
      success: false,
      error: {
        id: error.id,
        type: error.type,
        severity: error.severity,
        message: this.sanitizeMessage(error.message),
        timestamp: error.timestamp.toISOString()
      }
    };
  }

  /**
   * Wraps an async operation with error handling.
   * 
   * @template T - The return type of the operation
   * @param operation - The async operation to wrap
   * @param context - Error context for logging
   * @returns Result containing operation result or structured error
   */
  public async wrapAsync<T>(
    operation: () => Promise<T>,
    context?: Partial<ErrorContext>
  ): Promise<Result<T, AppError>> {
    try {
      const result = await operation();
      return ResultUtils.success(result);
    } catch (error) {
      const appError = this.handleUnexpectedError(error, context);
      return ResultUtils.error(appError);
    }
  }

  /**
   * Wraps a synchronous operation with error handling.
   * 
   * @template T - The return type of the operation
   * @param operation - The sync operation to wrap
   * @param context - Error context for logging
   * @returns Result containing operation result or structured error
   */
  public wrap<T>(
    operation: () => T,
    context?: Partial<ErrorContext>
  ): Result<T, AppError> {
    try {
      const result = operation();
      return ResultUtils.success(result);
    } catch (error) {
      const appError = this.handleUnexpectedError(error, context);
      return ResultUtils.error(appError);
    }
  }

  /**
   * Creates a validation error for invalid inputs.
   * 
   * @param message - Validation error message
   * @param field - Field that failed validation
   * @param context - Additional context
   * @returns Structured validation error
   */
  public createValidationError(
    message: string,
    field?: string,
    context?: Partial<ErrorContext>
  ): AppError {
    return this.createError({
      type: 'VALIDATION',
      severity: 'MEDIUM',
      message,
      details: field ? `Validation failed for field: ${field}` : undefined,
      context: {
        ...context,
        field
      }
    });
  }

  /**
   * Creates a security error for security violations.
   * 
   * @param message - Security error message
   * @param details - Security violation details
   * @param context - Additional context
   * @returns Structured security error
   */
  public createSecurityError(
    message: string,
    details?: string,
    context?: Partial<ErrorContext>
  ): AppError {
    return this.createError({
      type: 'SECURITY',
      severity: 'CRITICAL',
      message,
      details: this.isProduction ? 'Security violation detected' : details,
      context
    });
  }

  /**
   * Creates a business logic error.
   * 
   * @param message - Business error message
   * @param details - Business error details
   * @param context - Additional context
   * @returns Structured business error
   */
  public createBusinessError(
    message: string,
    details?: string,
    context?: Partial<ErrorContext>
  ): AppError {
    return this.createError({
      type: 'BUSINESS',
      severity: 'MEDIUM',
      message,
      details,
      context
    });
  }

  /**
   * Logs an error with appropriate security considerations.
   * 
   * @param error - The error to log
   */
  private logError(error: AppError): void {
    const logData = {
      id: error.id,
      type: error.type,
      severity: error.severity,
      message: error.message,
      timestamp: error.timestamp.toISOString(),
      context: this.sanitizeContext(error.context)
    };

    switch (error.severity) {
      case 'CRITICAL':
        this.logger.error('CRITICAL ERROR:', logData);
        break;
      case 'HIGH':
        this.logger.error('High severity error:', logData);
        break;
      case 'MEDIUM':
        this.logger.warn('Medium severity error:', logData);
        break;
      case 'LOW':
      default:
        this.logger.info('Low severity error:', logData);
        break;
    }
  }

  /**
   * Sanitizes error messages for production use.
   * 
   * @param message - The message to sanitize
   * @returns Sanitized message
   */
  private sanitizeMessage(message: string): string {
    if (!this.isProduction) {
      return message;
    }

    // In production, provide generic messages for security
    const secureMessages: Record<string, string> = {
      'VALIDATION': 'Input validation failed',
      'SECURITY': 'Security violation detected',
      'SYSTEM': 'System error occurred',
      'BUSINESS': 'Business logic error',
      'COMPLIANCE': 'Compliance violation detected'
    };

    // Return generic message if original message contains sensitive info
    if (this.containsSensitiveInfo(message)) {
      return 'An error occurred. Please check logs for details.';
    }

    return message;
  }

  /**
   * Sanitizes error context for logging.
   * 
   * @param context - The context to sanitize
   * @returns Sanitized context
   */
  private sanitizeContext(context?: Record<string, unknown>): Record<string, unknown> {
    if (!context) {
      return {};
    }

    const sanitized: Record<string, unknown> = {};
    
    for (const [key, value] of Object.entries(context)) {
      // Remove sensitive fields
      if (this.isSensitiveField(key)) {
        sanitized[key] = '[REDACTED]';
      } else if (typeof value === 'string' && this.containsSensitiveInfo(value)) {
        sanitized[key] = '[SANITIZED]';
      } else {
        sanitized[key] = value;
      }
    }

    return sanitized;
  }

  /**
   * Checks if a message contains sensitive information.
   * 
   * @param message - The message to check
   * @returns True if message contains sensitive info
   */
  private containsSensitiveInfo(message: string): boolean {
    const sensitivePatterns = [
      /password/i,
      /token/i,
      /secret/i,
      /key/i,
      /credential/i,
      /auth/i,
      /\b\d{3}-\d{2}-\d{4}\b/, // SSN pattern
      /\b\d{16}\b/, // Credit card pattern
      /api[_-]?key/i
    ];

    return sensitivePatterns.some(pattern => pattern.test(message));
  }

  /**
   * Checks if a field name indicates sensitive data.
   * 
   * @param fieldName - The field name to check
   * @returns True if field is sensitive
   */
  private isSensitiveField(fieldName: string): boolean {
    const sensitiveFields = [
      'password',
      'token',
      'secret',
      'key',
      'apiKey',
      'credential',
      'auth',
      'authorization',
      'ssn',
      'creditCard'
    ];

    return sensitiveFields.some(field => 
      fieldName.toLowerCase().includes(field.toLowerCase())
    );
  }
}
