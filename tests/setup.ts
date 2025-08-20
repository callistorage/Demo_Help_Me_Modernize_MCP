/**
 * Test setup configuration for Help Me Modernize test suite
 * Configures global test environment and utilities
 */

import { jest } from '@jest/globals';

// Mock environment variables for consistent testing
process.env.NODE_ENV = 'test';
process.env.ANTHROPIC_API_KEY = 'test-key-mock-12345';
process.env.LOG_LEVEL = 'error'; // Reduce logging noise in tests

// Global test timeout (can be overridden per test)
jest.setTimeout(30000);

// Mock Winston logger to prevent log pollution during tests
jest.mock('winston', () => ({
  createLogger: jest.fn(() => ({
    info: jest.fn(),
    error: jest.fn(),
    warn: jest.fn(),
    debug: jest.fn(),
    level: 'error'
  })),
  format: {
    combine: jest.fn(),
    timestamp: jest.fn(),
    errors: jest.fn(),
    json: jest.fn(),
    colorize: jest.fn(),
    simple: jest.fn(),
    printf: jest.fn()
  },
  transports: {
    Console: jest.fn(),
    File: jest.fn()
  }
}));

// Mock Anthropic API for consistent testing
const mockCreate = jest.fn() as jest.MockedFunction<any>;

jest.mock('@anthropic-ai/sdk', () => ({
  __esModule: true,
  default: jest.fn().mockImplementation(() => ({
    messages: {
      create: mockCreate
    }
  }))
}));

// Export mock for use in tests
export { mockCreate };

// Global test utilities
export const testUtils = {
  /**
   * Create a mock CodeFile for testing
   */
  createMockCodeFile: (overrides: Partial<any> = {}) => ({
    id: 'test-file-123',
    name: 'test.cob',
    content: 'IDENTIFICATION DIVISION.\nPROGRAM-ID. TEST-PROGRAM.',
    language: 'cobol' as const,
    size: 1024,
    uploadedAt: new Date('2025-01-01'),
    ...overrides
  }),

  /**
   * Create mock analysis request
   */
  createMockAnalysisRequest: (overrides: Partial<any> = {}) => ({
    files: [testUtils.createMockCodeFile()],
    analysisTypes: ['documentation'] as const,
    outputFormats: ['html'] as const,
    options: {
      detailLevel: 'detailed' as const,
      targetAudience: 'technical' as const
    },
    ...overrides
  }),

  /**
   * Delay helper for async testing
   */
  delay: (ms: number) => new Promise(resolve => setTimeout(resolve, ms))
};

// Global cleanup
afterEach(() => {
  jest.clearAllMocks();
});
