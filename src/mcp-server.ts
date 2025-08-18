/**
 * MCP Server Entry Point
 * Start the Help Me Modernize MCP server for government legacy code analysis
 */

import { HelpMeModernizeServer } from './mcp/HelpMeModernizeServer';
import { Logger } from './utils/Logger';

const logger = new Logger('MCPServerMain');

/**
 * Start the MCP server
 */
async function main(): Promise<void> {
  try {
    logger.info('Starting Help Me Modernize MCP Server...');
    
    const server = new HelpMeModernizeServer();
    await server.start();
    
    logger.info('MCP Server started successfully');
    
    // Handle graceful shutdown
    process.on('SIGINT', () => {
      logger.info('Received SIGINT, shutting down gracefully...');
      process.exit(0);
    });
    
    process.on('SIGTERM', () => {
      logger.info('Received SIGTERM, shutting down gracefully...');
      process.exit(0);
    });
    
  } catch (error) {
    logger.error('Failed to start MCP server:', error);
    process.exit(1);
  }
}

// Start the server
main();
