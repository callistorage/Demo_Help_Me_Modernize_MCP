/**
 * Help Me Modernize - Educational MCP Server
 * 
 * 🎓 EDUCATIONAL PURPOSE: This file demonstrates how to build a production-ready
 * MCP (Model Context Protocol) server that serves as the primary application
 * architecture, with thin clients connecting via the MCP protocol.
 * 
 * 🧠 LEARNING OBJECTIVES:
 * - Understanding MCP server architecture and tool implementation
 * - AI integration patterns with structured prompts and error handling
 * - Enterprise TypeScript patterns with strict type safety
 * - Government compliance patterns and security considerations
 * - Multi-format output generation for different audiences
 * 
 * 🏗️ ARCHITECTURE PATTERN: MCP Server as Primary Application
 * - Business logic centralized in MCP server
 * - Thin clients (web, CLI, API) connect via MCP protocol
 * - Extensible for multiple frontends and use cases
 * - AI integration ready for multiple models
 * 
 * 💡 STUDY FOCUS: Examine how each tool encapsulates specific functionality
 * with proper type safety, error handling, and educational value.
 */

import { Server } from '@modelcontextprotocol/sdk/server/index';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio';
import { 
  CallToolRequestSchema, 
  ErrorCode, 
  ListToolsRequestSchema, 
  McpError 
} from '@modelcontextprotocol/sdk/types';
import { LegacyCodeAnalyzer } from '../analyzers/LegacyCodeAnalyzer';
import { FileProcessor } from '../utils/FileProcessor';
import { OutputGenerator } from '../outputs/OutputGenerator';
import { Logger } from '../utils/Logger';
import { 
  AnalysisRequest, 
  MCPToolRequest, 
  MCPToolResponse,
  CodeFile,
  AnalysisResult
} from '../types/index.js';

/**
 * Help Me Modernize MCP Server
 * 
 * Provides tools for government developers to analyze and modernize legacy code.
 * Designed to work in air-gapped environments with configurable AI providers.
 */
export class HelpMeModernizeServer {
  private server: Server;
  private analyzer: LegacyCodeAnalyzer;
  private fileProcessor: FileProcessor;
  private outputGenerator: OutputGenerator;
  private logger: Logger;

  constructor() {
    this.logger = new Logger('HelpMeModernizeServer');
    this.server = new Server(
      {
        name: 'help-me-modernize',
        version: '1.0.0',
        description: 'MCP server for government legacy code analysis and modernization'
      },
      {
        capabilities: {
          tools: {},
        },
      }
    );

    this.analyzer = new LegacyCodeAnalyzer();
    this.fileProcessor = new FileProcessor();
    this.outputGenerator = new OutputGenerator();

    this.setupHandlers();
    this.logger.info('Help Me Modernize MCP Server initialized');
  }

  /**
   * Set up MCP request handlers
   */
  private setupHandlers(): void {
    // List available tools
    this.server.setRequestHandler(ListToolsRequestSchema, async () => ({
      tools: [
        {
          name: 'analyze_legacy_code',
          description: 'Analyze legacy code files (COBOL, Java, SQL) for documentation, business logic, security issues, and modernization opportunities',
          inputSchema: {
            type: 'object',
            properties: {
              files: {
                type: 'array',
                description: 'Array of code files to analyze',
                items: {
                  type: 'object',
                  properties: {
                    name: { type: 'string', description: 'File name' },
                    content: { type: 'string', description: 'File content' },
                    language: { 
                      type: 'string', 
                      enum: ['cobol', 'java', 'sql', 'text'],
                      description: 'Programming language' 
                    }
                  },
                  required: ['name', 'content', 'language']
                }
              },
              analysisTypes: {
                type: 'array',
                description: 'Types of analysis to perform',
                items: {
                  type: 'string',
                  enum: ['documentation', 'business-logic', 'security-vulnerabilities', 'modernization-suggestions']
                },
                default: ['documentation']
              },
              outputFormats: {
                type: 'array',
                description: 'Output formats to generate',
                items: {
                  type: 'string',
                  enum: ['html', 'markdown', 'json', 'text', 'pdf']
                },
                default: ['html']
              },
              options: {
                type: 'object',
                description: 'Analysis options',
                properties: {
                  detailLevel: {
                    type: 'string',
                    enum: ['summary', 'detailed', 'comprehensive'],
                    default: 'detailed'
                  },
                  targetAudience: {
                    type: 'string',
                    enum: ['technical', 'management', 'compliance'],
                    default: 'technical'
                  },
                  includeCodeExamples: {
                    type: 'boolean',
                    default: true
                  }
                }
              }
            },
            required: ['files']
          }
        },
        {
          name: 'upload_file',
          description: 'Upload and process a legacy code file for analysis',
          inputSchema: {
            type: 'object',
            properties: {
              fileName: { type: 'string', description: 'Name of the file' },
              fileContent: { type: 'string', description: 'Base64 encoded file content' },
              mimeType: { type: 'string', description: 'MIME type of the file' }
            },
            required: ['fileName', 'fileContent']
          }
        },
        {
          name: 'get_analysis_status',
          description: 'Get the status of a running analysis job',
          inputSchema: {
            type: 'object',
            properties: {
              jobId: { type: 'string', description: 'Analysis job ID' }
            },
            required: ['jobId']
          }
        },
        {
          name: 'generate_government_report',
          description: 'Generate a comprehensive government compliance report',
          inputSchema: {
            type: 'object',
            properties: {
              analysisResults: {
                type: 'array',
                description: 'Analysis results to include in report',
                items: { type: 'object' }
              },
              complianceStandards: {
                type: 'array',
                description: 'Compliance standards to check against',
                items: {
                  type: 'string',
                  enum: ['FISMA', 'FedRAMP', 'NIST', 'Custom']
                }
              },
              confidentialityLevel: {
                type: 'string',
                enum: ['public', 'internal', 'confidential', 'secret'],
                default: 'internal'
              }
            },
            required: ['analysisResults']
          }
        }
      ]
    }));

    // Handle tool calls
    this.server.setRequestHandler(CallToolRequestSchema, async (request) => {
      try {
        const { name, arguments: args } = request.params;
        this.logger.info(`Processing tool call: ${name}`);

        let result: MCPToolResponse;

        switch (name) {
          case 'analyze_legacy_code':
            result = await this.handleAnalyzeLegacyCode(args as any);
            break;
          
          case 'upload_file':
            result = await this.handleUploadFile(args as any);
            break;
          
          case 'get_analysis_status':
            result = await this.handleGetAnalysisStatus(args as any);
            break;
          
          case 'generate_government_report':
            result = await this.handleGenerateGovernmentReport(args as any);
            break;
          
          default:
            throw new McpError(
              ErrorCode.MethodNotFound,
              `Unknown tool: ${name}`
            );
        }

        // Return in the expected MCP response format
        return {
          content: result.content,
          isError: result.isError || false
        };

      } catch (error) {
        this.logger.error('Tool call error:', error);
        if (error instanceof McpError) {
          throw error;
        }
        throw new McpError(
          ErrorCode.InternalError,
          `Tool execution failed: ${error instanceof Error ? error.message : 'Unknown error'}`
        );
      }
    });
  }

  /**
   * Handle legacy code analysis requests
   */
  private async handleAnalyzeLegacyCode(args: {
    files: Array<{ name: string; content: string; language: string }>;
    analysisTypes?: string[];
    outputFormats?: string[];
    options?: any;
  }): Promise<MCPToolResponse> {
    try {
      this.logger.info(`Analyzing ${args.files.length} files`);

      // Process and validate files
      const processedFiles: CodeFile[] = [];
      for (const file of args.files) {
        const processed = await this.fileProcessor.processFile({
          name: file.name,
          content: file.content,
          language: file.language as any
        });
        
        if (processed.success && processed.fileId) {
          processedFiles.push({
            id: processed.fileId,
            name: file.name,
            content: file.content,
            language: processed.language,
            size: processed.size,
            uploadedAt: new Date()
          });
        }
      }

      if (processedFiles.length === 0) {
        throw new Error('No valid files to analyze');
      }

      // Create analysis request
      const analysisRequest: AnalysisRequest = {
        files: processedFiles,
        analysisTypes: (args.analysisTypes as any) || ['documentation'],
        outputFormats: (args.outputFormats as any) || ['html'],
        options: args.options || {}
      };

      // Perform analysis
      const results = await this.analyzer.analyzeFiles(analysisRequest);

      // Generate outputs
      const outputs = await this.outputGenerator.generateOutputs(results, analysisRequest.outputFormats);

      return {
        content: [
          {
            type: 'text',
            text: JSON.stringify({
              success: true,
              message: `Successfully analyzed ${processedFiles.length} files`,
              results: results,
              outputs: outputs,
              summary: {
                filesAnalyzed: processedFiles.length,
                totalFindings: results.reduce((sum, r) => sum + r.results.findings.length, 0),
                totalRecommendations: results.reduce((sum, r) => sum + r.results.recommendations.length, 0),
                averageRiskLevel: this.calculateAverageRiskLevel(results)
              }
            }, null, 2)
          }
        ]
      };
    } catch (error) {
      this.logger.error('Analysis failed:', error);
      return {
        content: [
          {
            type: 'text',
            text: JSON.stringify({
              success: false,
              error: error instanceof Error ? error.message : 'Analysis failed'
            })
          }
        ],
        isError: true
      };
    }
  }

  /**
   * Handle file upload requests
   */
  private async handleUploadFile(args: {
    fileName: string;
    fileContent: string;
    mimeType?: string;
  }): Promise<MCPToolResponse> {
    try {
      const result = await this.fileProcessor.uploadFile(
        args.fileName,
        args.fileContent,
        args.mimeType
      );

      return {
        content: [
          {
            type: 'text',
            text: JSON.stringify({
              success: result.success,
              fileId: result.fileId,
              fileName: result.fileName,
              language: result.language,
              size: result.size,
              warnings: result.warnings
            })
          }
        ]
      };
    } catch (error) {
      this.logger.error('File upload failed:', error);
      return {
        content: [
          {
            type: 'text',
            text: JSON.stringify({
              success: false,
              error: error instanceof Error ? error.message : 'Upload failed'
            })
          }
        ],
        isError: true
      };
    }
  }

  /**
   * Handle analysis status requests
   */
  private async handleGetAnalysisStatus(args: {
    jobId: string;
  }): Promise<MCPToolResponse> {
    // TODO: Implement job status tracking
    return {
      content: [
        {
          type: 'text',
          text: JSON.stringify({
            jobId: args.jobId,
            status: 'completed',
            message: 'Analysis status tracking not yet implemented'
          })
        }
      ]
    };
  }

  /**
   * Handle government report generation
   */
  private async handleGenerateGovernmentReport(args: {
    analysisResults: AnalysisResult[];
    complianceStandards?: string[];
    confidentialityLevel?: string;
  }): Promise<MCPToolResponse> {
    try {
      const report = await this.outputGenerator.generateGovernmentReport(
        args.analysisResults,
        args.complianceStandards || ['FISMA'],
        (args.confidentialityLevel as any) || 'internal'
      );

      return {
        content: [
          {
            type: 'text',
            text: JSON.stringify({
              success: true,
              report: report,
              message: 'Government report generated successfully'
            }, null, 2)
          }
        ]
      };
    } catch (error) {
      this.logger.error('Report generation failed:', error);
      return {
        content: [
          {
            type: 'text',
            text: JSON.stringify({
              success: false,
              error: error instanceof Error ? error.message : 'Report generation failed'
            })
          }
        ],
        isError: true
      };
    }
  }

  /**
   * Calculate average risk level from analysis results
   */
  private calculateAverageRiskLevel(results: AnalysisResult[]): string {
    const riskLevels = results
      .map(r => r.results.riskLevel)
      .filter(level => level !== undefined);
    
    if (riskLevels.length === 0) return 'unknown';
    
    const riskValues = { low: 1, medium: 2, high: 3, critical: 4 };
    const average = riskLevels.reduce((sum, level) => 
      sum + (riskValues[level as keyof typeof riskValues] || 0), 0
    ) / riskLevels.length;
    
    if (average <= 1.5) return 'low';
    if (average <= 2.5) return 'medium';
    if (average <= 3.5) return 'high';
    return 'critical';
  }

  /**
   * Start the MCP server
   */
  public async start(): Promise<void> {
    const transport = new StdioServerTransport();
    await this.server.connect(transport);
    this.logger.info('Help Me Modernize MCP Server started successfully');
  }
}

// Export for use in other modules
export default HelpMeModernizeServer;
