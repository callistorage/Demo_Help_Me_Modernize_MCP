/**
 * Logger utility for the Help Me Modernize application
 * Provides structured logging with different levels and government compliance considerations
 */

import winston from 'winston';
import fs from 'fs';
import path from 'path';

export class Logger {
  private winston: winston.Logger;
  private context: string;

  constructor(context: string = 'Application') {
    this.context = context;
    
    // Create Winston logger with government-appropriate configuration
    this.winston = winston.createLogger({
      level: process.env.LOG_LEVEL || 'info',
      format: winston.format.combine(
        winston.format.timestamp({
          format: 'YYYY-MM-DD HH:mm:ss'
        }),
        winston.format.errors({ stack: true }),
        winston.format.printf(({ level, message, timestamp, stack }) => {
          const logEntry = `${timestamp} [${this.context}] ${level.toUpperCase()}: ${message}`;
          return stack ? `${logEntry}\n${stack}` : logEntry;
        })
      ),
      transports: [
        // Console transport for development
        new winston.transports.Console({
          format: winston.format.combine(
            winston.format.colorize(),
            winston.format.simple()
          )
        }),
        
        // File transport for production/government environments
        new winston.transports.File({
          filename: 'logs/application.log',
          maxsize: 5242880, // 5MB
          maxFiles: 10,
          format: winston.format.combine(
            winston.format.timestamp(),
            winston.format.json()
          )
        }),
        
        // Error-specific log file
        new winston.transports.File({
          filename: 'logs/error.log',
          level: 'error',
          maxsize: 5242880, // 5MB
          maxFiles: 5,
          format: winston.format.combine(
            winston.format.timestamp(),
            winston.format.json()
          )
        })
      ],
      
      // Prevent crashes on uncaught exceptions
      exceptionHandlers: [
        new winston.transports.File({ filename: 'logs/exceptions.log' })
      ],
      
      // Handle promise rejections
      rejectionHandlers: [
        new winston.transports.File({ filename: 'logs/rejections.log' })
      ]
    });

    // Ensure logs directory exists
    this.ensureLogDirectory();
  }

  /**
   * Create logs directory if it doesn't exist
   */
  private ensureLogDirectory(): void {
    try {
      const logsDir = path.join(process.cwd(), 'logs');
      if (!fs.existsSync(logsDir)) {
        fs.mkdirSync(logsDir, { recursive: true });
      }
    } catch (error) {
      // If we can't create the logs directory, just log to console
      console.warn('Could not create logs directory, logging to console only:', error);
    }
  }

  /**
   * Log info level message
   */
  public info(message: string, meta?: any): void {
    this.winston.info(message, this.sanitizeMeta(meta));
  }

  /**
   * Log warning level message
   */
  public warn(message: string, meta?: any): void {
    this.winston.warn(message, this.sanitizeMeta(meta));
  }

  /**
   * Log error level message
   */
  public error(message: string, error?: Error | any): void {
    if (error instanceof Error) {
      this.winston.error(message, {
        error: {
          message: error.message,
          stack: error.stack,
          name: error.name
        }
      });
    } else {
      this.winston.error(message, this.sanitizeMeta(error));
    }
  }

  /**
   * Log debug level message
   */
  public debug(message: string, meta?: any): void {
    this.winston.debug(message, this.sanitizeMeta(meta));
  }

  /**
   * Log security-related events
   * Special handling for government compliance requirements
   */
  public security(message: string, meta?: any): void {
    this.winston.warn(`[SECURITY] ${message}`, this.sanitizeMeta(meta));
  }

  /**
   * Log compliance-related events
   */
  public compliance(message: string, meta?: any): void {
    this.winston.info(`[COMPLIANCE] ${message}`, this.sanitizeMeta(meta));
  }

  /**
   * Log audit trail events
   */
  public audit(action: string, userId?: string, meta?: any): void {
    const auditData = {
      action,
      userId: userId || 'anonymous',
      timestamp: new Date().toISOString(),
      ...this.sanitizeMeta(meta)
    };
    
    this.winston.info(`[AUDIT] ${action}`, auditData);
  }

  /**
   * Sanitize metadata to remove sensitive information
   * Critical for government applications to prevent data leaks
   */
  private sanitizeMeta(meta: any): any {
    if (!meta) return {};
    
    // Create a copy to avoid modifying original
    const sanitized = JSON.parse(JSON.stringify(meta));
    
    // Remove sensitive fields that might contain classified information
    const sensitiveFields = [
      'password', 'token', 'secret', 'key', 'auth',
      'ssn', 'social', 'classified', 'confidential',
      'api_key', 'apikey', 'authorization'
    ];
    
    this.removeSensitiveFields(sanitized, sensitiveFields);
    
    return sanitized;
  }

  /**
   * Recursively remove sensitive fields from objects
   */
  private removeSensitiveFields(obj: any, sensitiveFields: string[]): void {
    if (typeof obj !== 'object' || obj === null) return;
    
    for (const key in obj) {
      if (obj.hasOwnProperty(key)) {
        const lowerKey = key.toLowerCase();
        
        // Check if key contains sensitive information
        if (sensitiveFields.some(field => lowerKey.includes(field))) {
          obj[key] = '[REDACTED]';
        } else if (typeof obj[key] === 'object') {
          this.removeSensitiveFields(obj[key], sensitiveFields);
        }
      }
    }
  }

  /**
   * Create a child logger with additional context
   */
  public child(additionalContext: string): Logger {
    return new Logger(`${this.context}:${additionalContext}`);
  }

  /**
   * Set log level dynamically
   */
  public setLevel(level: string): void {
    this.winston.level = level;
  }

  /**
   * Get current log level
   */
  public getLevel(): string {
    return this.winston.level;
  }

  /**
   * Close logger and clean up resources
   */
  public close(): void {
    this.winston.close();
  }
}
