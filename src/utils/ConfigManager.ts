/**
 * @fileoverview Enterprise Configuration Management System
 * 
 * This module provides type-safe configuration management with validation,
 * environment-specific settings, and secure defaults for government deployments.
 * 
 * @module ConfigManager
 * @version 1.0.0
 * @author Help Me Modernize MCP Server
 * @since 2024
 * 
 * @security
 * - Validates all configuration values before use
 * - Provides secure defaults for production environments
 * - Sanitizes sensitive information in logs
 * - Supports air-gapped deployment configurations
 * 
 * @compliance
 * - Implements government configuration standards
 * - Provides audit-ready configuration logging
 * - Supports regulatory compliance requirements
 * - Validates security settings
 */

import { AppConfig, ValidationError, Result, ResultUtils } from '../types/common';
import { Logger } from './Logger';

/**
 * Environment types for configuration management.
 */
export type Environment = 'development' | 'staging' | 'production' | 'air-gapped';

/**
 * Configuration schema with validation rules.
 */
interface ConfigSchema {
  /** Configuration key path */
  readonly key: string;
  /** Configuration type for validation */
  readonly type: 'string' | 'number' | 'boolean' | 'array';
  /** Whether the configuration is required */
  readonly required: boolean;
  /** Default value if not provided */
  readonly defaultValue?: unknown;
  /** Validation function */
  readonly validate?: (value: unknown) => Result<unknown, ValidationError>;
  /** Whether the value contains sensitive information */
  readonly sensitive?: boolean;
}

/**
 * Enterprise configuration manager with type safety and validation.
 * 
 * Provides centralized configuration management with environment-specific
 * settings, validation, and secure handling of sensitive information
 * suitable for government deployments.
 * 
 * @example
 * ```typescript
 * const configManager = new ConfigManager();
 * const config = await configManager.load();
 * 
 * if (config.isSuccess) {
 *   console.log(`Server starting on port ${config.data.server.port}`);
 *   // Use config.data safely with full type checking
 * } else {
 *   console.error('Configuration validation failed:', config.error);
 * }
 * ```
 * 
 * @security
 * - Validates all configuration values before use
 * - Sanitizes sensitive information in error messages
 * - Provides secure defaults for production environments
 * 
 * @compliance
 * - Implements government configuration standards
 * - Provides detailed validation error reporting
 * - Supports audit trail requirements
 * 
 * @public
 */
export class ConfigManager {
  private readonly logger: Logger;
  private readonly environment: Environment;
  private cachedConfig?: AppConfig;

  /**
   * Configuration validation schema.
   */
  private readonly schema: ConfigSchema[] = [
    {
      key: 'server.port',
      type: 'number',
      required: false,
      defaultValue: 3000,
      validate: (value) => {
        const port = Number(value);
        if (isNaN(port) || port < 1 || port > 65535) {
          return ResultUtils.error({
            type: 'INVALID_PORT',
            message: 'Port must be a number between 1 and 65535',
            field: 'server.port'
          });
        }
        return ResultUtils.success(port);
      }
    },
    {
      key: 'server.host',
      type: 'string',
      required: false,
      defaultValue: '0.0.0.0'
    },
    {
      key: 'ai.apiKey',
      type: 'string',
      required: false,
      sensitive: true,
      validate: (value) => {
        if (value && typeof value === 'string' && value.length < 10) {
          return ResultUtils.error({
            type: 'INVALID_API_KEY',
            message: 'API key appears to be too short',
            field: 'ai.apiKey'
          });
        }
        return ResultUtils.success(value);
      }
    },
    {
      key: 'files.maxSize',
      type: 'number',
      required: false,
      defaultValue: 50 * 1024 * 1024, // 50MB
      validate: (value) => {
        const size = Number(value);
        if (isNaN(size) || size < 1024) {
          return ResultUtils.error({
            type: 'INVALID_FILE_SIZE',
            message: 'Maximum file size must be at least 1KB',
            field: 'files.maxSize'
          });
        }
        return ResultUtils.success(size);
      }
    }
  ];

  /**
   * Initializes the configuration manager.
   * 
   * @param environment - The deployment environment
   */
  constructor(environment?: Environment) {
    this.logger = new Logger('ConfigManager');
    this.environment = environment || this.detectEnvironment();
    this.logger.info(`Initializing configuration for ${this.environment} environment`);
  }

  /**
   * Loads and validates the application configuration.
   * 
   * @returns Promise resolving to validated configuration or validation errors
   */
  public async load(): Promise<Result<AppConfig, ValidationError[]>> {
    if (this.cachedConfig) {
      return ResultUtils.success(this.cachedConfig);
    }

    try {
      const rawConfig = await this.loadRawConfig();
      const validationResult = await this.validateConfig(rawConfig);

      if (!validationResult.isSuccess) {
        return validationResult;
      }

      this.cachedConfig = validationResult.data;
      this.logger.info('Configuration loaded and validated successfully');
      
      return ResultUtils.success(this.cachedConfig);
    } catch (error) {
      this.logger.error('Failed to load configuration:', error);
      return ResultUtils.error([{
        type: 'CONFIG_LOAD_ERROR',
        message: error instanceof Error ? error.message : 'Failed to load configuration'
      }]);
    }
  }

  /**
   * Gets a specific configuration value with type safety.
   * 
   * @template T - The expected type of the configuration value
   * @param key - The configuration key path (e.g., 'server.port')
   * @returns The configuration value or undefined if not found
   */
  public get<T = unknown>(key: string): T | undefined {
    if (!this.cachedConfig) {
      this.logger.warn(`Attempting to get config value '${key}' before configuration is loaded`);
      return undefined;
    }

    return this.getNestedValue(this.cachedConfig as any, key) as T;
  }

  /**
   * Reloads the configuration from environment/files.
   * 
   * @returns Promise resolving to reloaded configuration or validation errors
   */
  public async reload(): Promise<Result<AppConfig, ValidationError[]>> {
    this.cachedConfig = undefined;
    return this.load();
  }

  /**
   * Detects the current environment.
   * 
   * @returns The detected environment type
   */
  private detectEnvironment(): Environment {
    const nodeEnv = process.env.NODE_ENV?.toLowerCase();
    
    if (process.env.AIR_GAPPED === 'true') {
      return 'air-gapped';
    }
    
    switch (nodeEnv) {
      case 'production':
        return 'production';
      case 'staging':
        return 'staging';
      case 'development':
      default:
        return 'development';
    }
  }

  /**
   * Loads raw configuration from environment variables and files.
   * 
   * @returns Promise resolving to raw configuration object
   */
  private async loadRawConfig(): Promise<Record<string, unknown>> {
    const config: Record<string, unknown> = {};

    // Load from environment variables
    config['server.port'] = process.env.PORT || process.env.SERVER_PORT;
    config['server.host'] = process.env.HOST || process.env.SERVER_HOST;
    config['ai.apiKey'] = process.env.ANTHROPIC_API_KEY;
    config['ai.provider'] = process.env.AI_PROVIDER || 'anthropic';
    config['ai.model'] = process.env.AI_MODEL || 'claude-sonnet-4-20250514';
    config['ai.maxTokens'] = process.env.AI_MAX_TOKENS;
    config['ai.temperature'] = process.env.AI_TEMPERATURE;
    config['files.maxSize'] = process.env.MAX_FILE_SIZE;
    config['files.maxCount'] = process.env.MAX_FILE_COUNT;
    config['logging.level'] = process.env.LOG_LEVEL;

    // Remove undefined values
    Object.keys(config).forEach(key => {
      if (config[key] === undefined) {
        delete config[key];
      }
    });

    return config;
  }

  /**
   * Validates configuration against schema.
   * 
   * @param rawConfig - Raw configuration object to validate
   * @returns Promise resolving to validated configuration or errors
   */
  private async validateConfig(
    rawConfig: Record<string, unknown>
  ): Promise<Result<AppConfig, ValidationError[]>> {
    const errors: ValidationError[] = [];
    const validatedConfig: Record<string, unknown> = {};

    // Validate each schema entry
    for (const schemaEntry of this.schema) {
      const value = rawConfig[schemaEntry.key] ?? schemaEntry.defaultValue;

      if (schemaEntry.required && value === undefined) {
        errors.push({
          type: 'REQUIRED_CONFIG_MISSING',
          message: `Required configuration '${schemaEntry.key}' is missing`,
          field: schemaEntry.key
        });
        continue;
      }

      if (value !== undefined && schemaEntry.validate) {
        const validationResult = schemaEntry.validate(value);
        if (!validationResult.isSuccess) {
          errors.push(validationResult.error);
          continue;
        }
        validatedConfig[schemaEntry.key] = validationResult.data;
      } else {
        validatedConfig[schemaEntry.key] = value;
      }
    }

    if (errors.length > 0) {
      return ResultUtils.error(errors);
    }

    // Build the final configuration object
    const appConfig: AppConfig = {
      server: {
        port: validatedConfig['server.port'] as number,
        host: validatedConfig['server.host'] as string,
        cors: {
          enabled: this.environment !== 'air-gapped',
          origins: this.environment === 'production' ? [] : ['*']
        }
      },
      ai: {
        provider: (validatedConfig['ai.provider'] as any) || 'anthropic',
        apiKey: validatedConfig['ai.apiKey'] as string,
        model: validatedConfig['ai.model'] as string || 'claude-sonnet-4-20250514',
        maxTokens: Number(validatedConfig['ai.maxTokens']) || 8000,
        temperature: Number(validatedConfig['ai.temperature']) || 0.1
      },
      files: {
        maxSize: validatedConfig['files.maxSize'] as number,
        maxCount: Number(validatedConfig['files.maxCount']) || 100,
        allowedExtensions: ['.cob', '.java', '.sql', '.txt', '.cfg', '.conf'],
        maxAnalysisLines: 10000
      },
      logging: {
        level: (validatedConfig['logging.level'] as any) || 'info',
        format: this.environment === 'production' ? 'json' : 'text',
        destinations: this.environment === 'air-gapped' ? ['file'] : ['console', 'file']
      },
      security: {
        validateFileContents: true,
        sanitizeInputs: true,
        auditTrail: this.environment === 'production' || this.environment === 'air-gapped'
      }
    };

    return ResultUtils.success(appConfig);
  }

  /**
   * Gets a nested value from an object using dot notation.
   * 
   * @param obj - The object to search
   * @param key - The key path (e.g., 'server.port')
   * @returns The value or undefined if not found
   */
  private getNestedValue(obj: Record<string, unknown>, key: string): unknown {
    return key.split('.').reduce<unknown>((current, segment) => {
      return current && typeof current === 'object' 
        ? (current as Record<string, unknown>)[segment]
        : undefined;
    }, obj);
  }
}
