/**
 * Main server entry point
 * Combines MCP server with simple web interface for government users
 */

import 'dotenv/config';
import express from 'express';
import cors from 'cors';
import helmet from 'helmet';
import multer from 'multer';
import path from 'path';
import { fileURLToPath } from 'url';
import { HelpMeModernizeServer } from './mcp/HelpMeModernizeServer.js';
import { Logger } from './utils/Logger.js';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const logger = new Logger('WebServer');

// Configuration
const PORT = process.env.PORT || 3000;
const NODE_ENV = process.env.NODE_ENV || 'development';

/**
 * Express web server for the MCP-based legacy code analyzer
 * Provides a simple web interface for government users
 */
class WebServer {
  private app: express.Application;
  private mcpServer: HelpMeModernizeServer;

  constructor() {
    this.app = express();
    this.mcpServer = new HelpMeModernizeServer();
    this.setupMiddleware();
    this.setupRoutes();
  }

  /**
   * Setup Express middleware
   */
  private setupMiddleware(): void {
    // Security middleware
    this.app.use(helmet({
      contentSecurityPolicy: {
        directives: {
          defaultSrc: ["'self'"],
          styleSrc: ["'self'", "'unsafe-inline'"],
          scriptSrc: ["'self'", "'unsafe-inline'"],
          scriptSrcAttr: ["'unsafe-inline'"],  // Allow inline event handlers
          imgSrc: ["'self'", "data:"],
        },
      },
    }));

    // CORS configuration for government environments
    this.app.use(cors({
      origin: NODE_ENV === 'development' ? true : false,
      credentials: true
    }));

    // Body parsing
    this.app.use(express.json({ limit: '50mb' }));
    this.app.use(express.urlencoded({ extended: true, limit: '50mb' }));

    // File upload configuration
    const upload = multer({
      storage: multer.memoryStorage(),
      limits: {
        fileSize: 50 * 1024 * 1024, // 50MB
        files: 100
      },
      fileFilter: (req, file, cb) => {
        // Allow specific file types for government legacy code
        const allowedTypes = /\.(cob|cbl|cobol|cpy|copy|java|class|sql|ddl|dml|proc|sp|txt|log|cfg|conf|ini|properties)$/i;
        if (allowedTypes.test(file.originalname)) {
          cb(null, true);
        } else {
          cb(new Error('File type not supported for legacy code analysis'));
        }
      }
    });

    this.app.use('/upload', upload.array('files', 100));

    // Static files for web interface
    this.app.use(express.static(path.join(__dirname, 'public')));

    // Request logging
    this.app.use((req, res, next) => {
      logger.info(`${req.method} ${req.path} - ${req.ip}`);
      next();
    });
  }

  /**
   * Setup Express routes
   */
  private setupRoutes(): void {
    // Health check endpoint
    this.app.get('/health', (req, res) => {
      res.json({
        status: 'healthy',
        timestamp: new Date().toISOString(),
        version: '1.0.0',
        service: 'Help Me Modernize'
      });
    });

    // Main web interface - serve static HTML
    this.app.get('/', (req, res) => {
      res.sendFile(path.join(__dirname, 'public', 'index.html'));
    });

    // API endpoint for file analysis
    this.app.post('/api/analyze', async (req, res) => {
      try {
        const { files, analysisTypes, outputFormats, options } = req.body;

        // Validate files
        if (!files || !Array.isArray(files) || files.length === 0) {
          return res.status(400).json({
            success: false,
            error: 'No files provided for analysis'
          });
        }

        if (files.length > 100) {
          return res.status(400).json({
            success: false,
            error: 'Maximum 100 files allowed per request'
          });
        }

        // Validate analysis types
        const validAnalysisTypes = ['documentation', 'business-logic', 'security-vulnerabilities', 'modernization-suggestions'];
        if (analysisTypes && !Array.isArray(analysisTypes)) {
          return res.status(400).json({
            success: false,
            error: 'analysisTypes must be an array'
          });
        }
        
        if (analysisTypes && analysisTypes.some((type: any) => !validAnalysisTypes.includes(type))) {
          return res.status(400).json({
            success: false,
            error: 'Invalid analysis type. Valid types: ' + validAnalysisTypes.join(', ')
          });
        }

        // Validate output formats
        const validOutputFormats = ['html', 'markdown', 'json', 'text', 'pdf'];
        if (outputFormats && !Array.isArray(outputFormats)) {
          return res.status(400).json({
            success: false,
            error: 'outputFormats must be an array'
          });
        }
        
        if (outputFormats && outputFormats.some((format: any) => !validOutputFormats.includes(format))) {
          return res.status(400).json({
            success: false,
            error: 'Invalid output format. Valid formats: ' + validOutputFormats.join(', ')
          });
        }

        // Use MCP server tools to analyze files
        const result = await this.mcpServer['handleAnalyzeLegacyCode']({
          files,
          analysisTypes: analysisTypes || ['documentation'],
          outputFormats: outputFormats || ['html'],
          options: options || {}
        });

        // Parse the result from MCP response
        const mcpResponse = JSON.parse(result.content[0].text || '{}');
        
        return res.json(mcpResponse);

      } catch (error) {
        logger.error('Analysis request failed:', error);
        return res.status(500).json({
          success: false,
          error: error instanceof Error ? error.message : 'Analysis failed'
        });
      }
    });

    // File upload endpoint
    this.app.post('/api/upload', (req, res) => {
      try {
        const files = req.files as Express.Multer.File[];
        
        if (!files || files.length === 0) {
          return res.status(400).json({
            success: false,
            error: 'No files uploaded'
          });
        }

        const processedFiles = files.map(file => ({
          name: file.originalname,
          content: file.buffer.toString('utf-8'),
          size: file.size,
          mimeType: file.mimetype
        }));

        return res.json({
          success: true,
          message: `Successfully uploaded ${files.length} files`,
          files: processedFiles
        });

      } catch (error) {
        logger.error('File upload failed:', error);
        return res.status(500).json({
          success: false,
          error: error instanceof Error ? error.message : 'Upload failed'
        });
      }
    });

    // API information endpoint
    this.app.get('/api/info', (req, res) => {
      res.json({
        name: 'Help Me Modernize',
        version: '1.0.0',
        description: 'Government legacy code analysis and modernization tool',
        supportedLanguages: ['cobol', 'java', 'sql', 'text'],
        supportedFormats: ['html', 'markdown', 'json', 'text', 'pdf'],
        analysisTypes: [
          'documentation',
          'business-logic', 
          'security-vulnerabilities',
          'modernization-suggestions'
        ],
        maxFileSize: '50MB',
        maxFiles: 100
      });
    });

    // Error handling
    this.app.use((err: Error, req: express.Request, res: express.Response, next: express.NextFunction) => {
      logger.error('Unhandled error:', err);
      res.status(500).json({
        success: false,
        error: NODE_ENV === 'development' ? err.message : 'Internal server error'
      });
    });

    // 404 handler
    this.app.use((req, res) => {
      res.status(404).json({
        success: false,
        error: 'Endpoint not found'
      });
    });
  }

  /**
   * Start the web server
   */
  public async start(): Promise<void> {
    try {
      // Start MCP server (in background for this demo)
      // In production, this would be a separate process
      logger.info('Starting MCP server...');
      
      // Start Express server
      this.app.listen(PORT, () => {
        logger.info(`Help Me Modernize web server started on port ${PORT}`);
        logger.info(`Environment: ${NODE_ENV}`);
        logger.info(`Web interface: http://localhost:${PORT}`);
        logger.info(`API endpoint: http://localhost:${PORT}/api`);
        logger.info('Help Me Modernize is ready for government legacy code analysis!');
      });

    } catch (error) {
      logger.error('Failed to start web server:', error);
      process.exit(1);
    }
  }
}

// Start the server if this file is run directly
// For ES modules, we need to check if this is the main module being executed
const isMainModule = import.meta.url === `file://${process.argv[1]}` || 
                     __filename === path.resolve(process.argv[1]) ||
                     process.argv[1]?.endsWith('server.js') ||
                     process.argv[1]?.endsWith('server.ts');

if (isMainModule) {
  const server = new WebServer();
  server.start().catch(error => {
    console.error('Failed to start server:', error);
    process.exit(1);
  });
}

export default WebServer;
