/**
 * @fileoverview Common Types and Utilities for Enterprise TypeScript
 * 
 * This module provides enterprise-grade common types, Result patterns, and utility
 * types for robust error handling and type safety throughout the application.
 * 
 * @module CommonTypes
 * @version 1.0.0
 * @author Help Me Modernize MCP Server
 * @since 2024
 * 
 * @security
 * - Type-safe error handling prevents information leakage
 * - Structured result types ensure consistent error reporting
 * - Generic types provide compile-time safety
 * 
 * @compliance
 * - Implements government-grade error handling standards
 * - Provides audit-trail compatible error structures
 * - Supports regulatory reporting requirements
 */

/**
 * Enterprise Result type for functional error handling.
 * 
 * Provides a type-safe way to handle operations that can succeed or fail,
 * eliminating the need for thrown exceptions in most cases and providing
 * structured error information for government compliance.
 * 
 * @template T - The success result type
 * @template E - The error type (defaults to string)
 * 
 * @example
 * ```typescript
 * async function validateFile(content: string): Promise<Result<FileInfo, ValidationError>> {
 *   if (!content) {
 *     return ResultUtils.error({ type: 'EMPTY_CONTENT', message: 'File content is empty' });
 *   }
 *   
 *   return ResultUtils.success({ size: content.length, type: 'valid' });
 * }
 * 
 * const result = await validateFile(fileContent);
 * if (result.isSuccess) {
 *   console.log(`File size: ${result.data.size}`);
 * } else {
 *   console.error(`Validation failed: ${result.error.message}`);
 * }
 * ```
 */
export type Result<T, E = string> = 
  | { isSuccess: true; data: T; error?: never }
  | { isSuccess: false; data?: never; error: E };

/**
 * ResultUtils utility class for creating and working with Result types.
 * 
 * Provides static methods for creating success and error results,
 * and utility methods for working with Result types in a functional manner.
 */
export class ResultUtils {
  /**
   * Creates a successful Result.
   * 
   * @template T - The success data type
   * @param data - The success data
   * @returns Result representing success
   */
  static success<T>(data: T): Result<T, never> {
    return { isSuccess: true, data };
  }

  /**
   * Creates a failed Result.
   * 
   * @template E - The error type
   * @param error - The error information
   * @returns Result representing failure
   */
  static error<E>(error: E): Result<never, E> {
    return { isSuccess: false, error };
  }

  /**
   * Maps a successful Result to a new type.
   * 
   * @template T - The original success type
   * @template U - The new success type
   * @template E - The error type
   * @param result - The result to map
   * @param mapper - Function to transform the success data
   * @returns New Result with transformed data
   */
  static map<T, U, E>(
    result: Result<T, E>,
    mapper: (data: T) => U
  ): Result<U, E> {
    return result.isSuccess
      ? ResultUtils.success(mapper(result.data))
      : result;
  }

  /**
   * Chains Result operations together.
   * 
   * @template T - The original success type
   * @template U - The new success type
   * @template E - The error type
   * @param result - The result to chain from
   * @param mapper - Function that returns a new Result
   * @returns New Result from the mapper function
   */
  static flatMap<T, U, E>(
    result: Result<T, E>,
    mapper: (data: T) => Result<U, E>
  ): Result<U, E> {
    return result.isSuccess ? mapper(result.data) : result;
  }
}

/**
 * Validation error structure for government compliance.
 * 
 * Provides structured error information suitable for audit trails
 * and compliance reporting in government environments.
 */
export interface ValidationError {
  /** Error type identifier for categorization */
  readonly type: string;
  /** Human-readable error message */
  readonly message: string;
  /** Optional field that caused the validation error */
  readonly field?: string;
  /** Optional error code for system integration */
  readonly code?: string;
  /** Additional context for debugging */
  readonly context?: Record<string, unknown>;
}

/**
 * Structured application error for enterprise error handling.
 * 
 * Provides comprehensive error information suitable for logging,
 * monitoring, and compliance reporting in government systems.
 */
export interface AppError {
  /** Unique error identifier */
  readonly id: string;
  /** Error type for categorization */
  readonly type: 'VALIDATION' | 'BUSINESS' | 'SYSTEM' | 'SECURITY' | 'COMPLIANCE';
  /** Error severity level */
  readonly severity: 'LOW' | 'MEDIUM' | 'HIGH' | 'CRITICAL';
  /** Human-readable error message */
  readonly message: string;
  /** Technical details for debugging */
  readonly details?: string;
  /** Timestamp when error occurred */
  readonly timestamp: Date;
  /** Context information for debugging */
  readonly context?: Record<string, unknown>;
  /** Original error if this wraps another error */
  readonly cause?: Error;
}

/**
 * Configuration interface for application settings.
 * 
 * Provides type-safe configuration management with validation
 * and environment-specific settings for government deployments.
 */
export interface AppConfig {
  /** Server configuration */
  readonly server: {
    readonly port: number;
    readonly host: string;
    readonly cors: {
      readonly enabled: boolean;
      readonly origins: string[];
    };
  };
  
  /** AI service configuration */
  readonly ai: {
    readonly provider: 'anthropic' | 'mock';
    readonly apiKey?: string;
    readonly model: string;
    readonly maxTokens: number;
    readonly temperature: number;
  };
  
  /** File processing limits */
  readonly files: {
    readonly maxSize: number;
    readonly maxCount: number;
    readonly allowedExtensions: string[];
    readonly maxAnalysisLines: number;
  };
  
  /** Logging configuration */
  readonly logging: {
    readonly level: 'debug' | 'info' | 'warn' | 'error';
    readonly format: 'json' | 'text';
    readonly destinations: Array<'console' | 'file' | 'syslog'>;
  };
  
  /** Security settings */
  readonly security: {
    readonly validateFileContents: boolean;
    readonly sanitizeInputs: boolean;
    readonly auditTrail: boolean;
  };
}

/**
 * Utility type for making properties optional.
 */
export type Optional<T, K extends keyof T> = Omit<T, K> & Partial<Pick<T, K>>;

/**
 * Utility type for deep readonly.
 */
export type DeepReadonly<T> = {
  readonly [P in keyof T]: T[P] extends object ? DeepReadonly<T[P]> : T[P];
};

/**
 * Utility type for non-empty arrays.
 */
export type NonEmptyArray<T> = [T, ...T[]];

/**
 * Branded type for type-safe identifiers.
 * 
 * @template T - The base type (usually string)
 * @template Brand - The brand identifier
 */
export type Branded<T, Brand> = T & { readonly __brand: Brand };

/** Type-safe file ID */
export type FileId = Branded<string, 'FileId'>;

/** Type-safe job ID */
export type JobId = Branded<string, 'JobId'>;

/** Type-safe user ID */
export type UserId = Branded<string, 'UserId'>;
