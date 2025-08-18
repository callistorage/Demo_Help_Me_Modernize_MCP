# 🏛️ Help Me Modernize

**MCP-Powered Legacy Code Documentation and Modernization Tool for Government Developers**

[![TypeScript](https://img.shields.io/badge/TypeScript-5.2-blue.svg)](https://www.typescriptlang.org/)
[![MCP](https://img.shields.io/badge/MCP-Model%20Context%20Protocol-green.svg)](https://modelcontextprotocol.io/)
[![Claude](https://img.shields.io/badge/AI-Claude%204-purple.svg)](https://www.anthropic.com/)
[![Government](https://img.shields.io/badge/Use%20Case-Government-red.svg)](https://github.com)

## 📋 Overview

"Help Me Modernize" is a comprehensive TypeScript-based tool that uses the Model Context Protocol (MCP) to help government developers understand, document, and improve legacy code systems. This project demonstrates advanced MCP integration patterns while addressing real-world challenges in government legacy system modernization.

### 🎯 Key Features

- **🤖 AI-Powered Analysis**: Uses Claude 4 (latest 2025 model) for advanced code understanding. *Note: Government deployments should use Claude 3.5 Sonnet as the most recent FedRAMP-authorized model*
- **🛡️ Government Security**: Built for air-gapped environments with security best practices  
- **📊 Multi-Format Output**: HTML, Markdown, JSON, Text, and PDF reports
- **🏗️ MCP Architecture**: Demonstrates MCP server as primary application pattern
- **📋 Compliance Ready**: Supports FISMA, FedRAMP, and NIST requirements
- **🔧 Legacy Focus**: Specialized for COBOL, legacy Java, SQL, and government systems

### 🏛️ Government Use Cases

- **Code Documentation**: Convert complex legacy code into plain English explanations
- **Business Logic Extraction**: Identify and document core business rules and processes  
- **Security Assessment**: Find vulnerabilities and compliance issues
- **Modernization Planning**: Get practical recommendations for system upgrades
- **Knowledge Transfer**: Preserve institutional knowledge before retirements
- **Compliance Reporting**: Generate audit-ready documentation

## 🚀 Quick Start

### Prerequisites

- **Node.js 18+**
- **TypeScript 5.2+** 
- **Anthropic API Key** (for AI analysis)

### Installation

```bash
# Clone the repository
git clone <repository-url>
cd help-me-modernize

# Install dependencies
npm install

# Build the project
npm run build

# Set up environment variables
cp .env.example .env
# Edit .env with your Anthropic API key
```

### Configuration

Create a `.env` file with your configuration:

```env
# AI Configuration
ANTHROPIC_API_KEY=your_anthropic_api_key_here

# Server Configuration  
PORT=3000
NODE_ENV=development
LOG_LEVEL=info

# Government Security (Future)
# CLASSIFICATION_LEVEL=unclassified
# DEPLOYMENT_MODE=air_gapped
```

### AI Model Configuration

This project uses **Claude 4** (latest 2025 model) for optimal legacy code analysis capabilities. However, for government deployments, model selection should consider compliance requirements:

#### 🏛️ **Government Production Use**
```env
# Use FedRAMP-authorized model (recommended for government)
CLAUDE_MODEL=claude-3-5-sonnet-20241022
```

#### 🧪 **Development/Demo Use** 
```env
# Use latest model for enhanced capabilities
CLAUDE_MODEL=claude-sonnet-4-20250514
```

#### **Model Comparison**:
- **Claude 4**: Enhanced COBOL understanding, improved government compliance analysis, longer context window
- **Claude 3.5 Sonnet**: FedRAMP authorized, production-ready for government environments
- **Fallback**: Mock responses available when no API key is provided

> **Government Note**: Always verify current FedRAMP authorization status for AI models before production deployment. The tool is designed to easily switch between authorized models via configuration.

> **Security Note**: The development configuration uses `'unsafe-inline'` CSP for simplicity. For production government deployment, implement nonce-based CSP. See [Security Considerations](#security-considerations) section for details.

### Running the Application

#### Option 1: Web Interface (Recommended for Demo)

```bash
# Start the web server with integrated MCP
npm run dev

# Access the web interface
open http://localhost:3000
```

#### Option 2: Pure MCP Server

```bash
# Start standalone MCP server
npm run start:mcp

# Connect with MCP client tools
```

## 🏗️ Architecture

### MCP Server as Primary Application

This project implements the **MCP Server as Primary Application** pattern, where the MCP server contains all business logic and the web interface is a thin client.

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Web Client    │◄──►│   MCP Server    │◄──►│   Claude API    │
│                 │    │                 │    │                 │
│ • File Upload   │    │ • File Analysis │    │ • AI Analysis   │
│ • Progress UI   │    │ • Output Gen    │    │ • Code Review   │
│ • Results View  │    │ • Government    │    │ • Suggestions   │
│                 │    │   Compliance    │    │                 │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

### Project Structure

```
src/
├── mcp/                    # MCP Server Implementation
│   └── HelpMeModernizeServer.ts
├── analyzers/              # Code Analysis Engine
│   └── LegacyCodeAnalyzer.ts
├── outputs/                # Multi-Format Output Generation
│   └── OutputGenerator.ts
├── utils/                  # Utilities
│   ├── Logger.ts          # Government-compliant logging
│   └── FileProcessor.ts   # Secure file handling
├── types/                  # TypeScript Type Definitions
│   └── index.ts
├── web/                    # Web Interface
└── server.ts              # Web Server + MCP Integration
```

## 🔧 Usage

### Analyzing Legacy Code

#### Via Web Interface

1. **Upload Files**: Select COBOL, Java, SQL, or text files
2. **Choose Analysis**: Select analysis types and output formats
3. **Review Results**: Download reports and review findings

#### Via API

```typescript
// POST /api/analyze
{
  "files": [
    {
      "name": "payroll.cob",
      "content": "IDENTIFICATION DIVISION...",
      "language": "cobol"
    }
  ],
  "analysisTypes": ["documentation", "business-logic"],
  "outputFormats": ["html", "json"],
  "options": {
    "detailLevel": "comprehensive",
    "targetAudience": "technical"
  }
}
```

#### Via MCP Tools

```typescript
// Call MCP tool directly
await mcpServer.callTool("analyze_legacy_code", {
  files: [...],
  analysisTypes: ["security-vulnerabilities"],
  outputFormats: ["pdf"]
});
```

### Analysis Types

1. **📖 Code Documentation**
   - Convert legacy code to plain English
   - Explain business logic and data flows
   - Generate comprehensive code comments

2. **🧠 Business Logic Extraction**
   - Identify core business rules
   - Document decision points and validations
   - Map data transformations

3. **🛡️ Security Vulnerability Assessment**
   - Find potential security issues
   - Check government compliance standards
   - Risk assessment and mitigation recommendations

4. **🔄 Modernization Suggestions**
   - Practical modernization roadmap
   - Technology migration strategies
   - Risk-assessed improvement plans

### Output Formats

- **📄 HTML**: Rich interactive reports with navigation
- **📝 Markdown**: Developer-friendly, version-controllable
- **📊 JSON**: API integration and data processing
- **📄 Text**: Simple reports for email and documentation
- **📑 PDF**: Executive summaries and formal reports

## 🛡️ Government Security & Compliance

### Air-Gapped Deployment

For secure government environments, the tool supports air-gapped deployment:

```bash
# Build for air-gapped deployment
npm run build:airgap

# Package for offline installation
npm run package:offline
```

#### Air-Gap Conversion Requirements

To convert from cloud-based to air-gapped deployment:

1. **Remove External API Calls**
   - Replace Anthropic API with local AI models
   - Configure offline language models (Ollama, etc.)

2. **Local File Processing**
   - Disable external file upload services
   - Implement local-only file processing

3. **Network Isolation**
   - Remove internet connectivity requirements
   - Configure internal-only services

4. **Security Hardening**
   - Enable additional audit logging
   - Implement government authentication systems
   - Add data classification handling

### Compliance Standards

The tool includes placeholders and documentation for:

- **🏛️ FISMA (Federal Information Security Management Act)**
- **☁️ FedRAMP (Federal Risk and Authorization Management Program)**
- **🔒 NIST Cybersecurity Framework**
- **📋 Custom Government Standards**

### Security Features

- **🔐 Secure File Handling**: Validates uploads, prevents directory traversal
- **📝 Audit Logging**: Comprehensive activity logs for compliance
- **🛡️ Input Sanitization**: Prevents injection attacks and data leaks
- **🚫 Data Retention**: Configurable data handling policies

### Security Considerations

#### Content Security Policy (CSP)
- **Current Configuration**: Allows inline scripts for development simplicity
- **Production Recommendation**: Implement nonce-based CSP for enhanced security
- **Trade-off**: Balanced security vs. development velocity for government use case

#### Multi-Layer Input Validation
- **Client-side**: Immediate feedback for file size and type validation
- **Server-side**: File type filtering, content validation, size limits
- **API-level**: Parameter validation with whitelisted values
- **Processor-level**: Binary detection, dangerous filename checking

#### Government Security Compliance
- **Air-gap Ready**: Designed for disconnected government environments
- **No External Dependencies**: All processing happens locally
- **Audit Trail**: All operations logged for compliance review
- **Data Isolation**: Files processed in memory, no persistent storage

#### Production Hardening Recommendations
```typescript
// Enhanced CSP for production deployment
contentSecurityPolicy: {
  directives: {
    defaultSrc: ["'self'"],
    scriptSrc: ["'self'", "'nonce-{random}'"], // Use nonces instead of unsafe-inline
    connectSrc: ["'self'"],                    // API calls only to same origin
    objectSrc: ["'none'"],                     // Block plugins
    baseUri: ["'self'"],                       // Prevent base tag injection
  }
}
```

#### Security Documentation
- **Threat Model**: File upload and processing in government environment
- **Risk Assessment**: Low-medium risk due to controlled user base
- **Mitigation Strategy**: Defense in depth with multiple validation layers
- **Future Enhancements**: Rate limiting, CSRF protection, enhanced CSP

## 🧪 Development

### Development Setup

```bash
# Install dependencies
npm install

# Start development server with hot reload
npm run dev

# Run tests
npm test

# Lint code
npm run lint

# Type checking
npm run build
```

### Adding New Analysis Types

1. **Update Types**: Add new analysis type to `src/types/index.ts`
2. **Implement Analyzer**: Add analysis logic in `LegacyCodeAnalyzer.ts`
3. **Update MCP Tools**: Register new tool in `HelpMeModernizeServer.ts`
4. **Add UI Support**: Update web interface as needed

### Testing

```bash
# Run all tests
npm test

# Run specific test suites
npm test -- --grep "LegacyCodeAnalyzer"

# Run with coverage
npm run test:coverage
```

## 📚 MCP Integration Guide

### Understanding the MCP Architecture

This project demonstrates advanced MCP patterns:

#### 1. **MCP Server as Primary Application**
- Business logic lives in MCP server
- Thin clients communicate via MCP protocol
- Extensible architecture for multiple frontends

#### 2. **Government-Specific MCP Tools**
- `analyze_legacy_code`: Core analysis functionality
- `upload_file`: Secure file processing
- `generate_government_report`: Compliance reporting
- `get_analysis_status`: Progress tracking

#### 3. **Multi-Model AI Integration**
- Designed for Claude API but extensible
- Support for local models in air-gapped environments
- Configurable AI providers through environment variables

### Extending the MCP Server

```typescript
// Add new MCP tool
this.server.setRequestHandler(ListToolsRequestSchema, async () => ({
  tools: [
    {
      name: 'your_new_tool',
      description: 'Your tool description',
      inputSchema: {
        // JSON schema for inputs
      }
    }
  ]
}));

// Handle tool calls
this.server.setRequestHandler(CallToolRequestSchema, async (request) => {
  if (request.params.name === 'your_new_tool') {
    return await this.handleYourNewTool(request.params.arguments);
  }
});
```

## 🚀 Deployment

### Development Deployment

```bash
# Start development server
npm run dev
```

### Production Deployment

```bash
# Build for production
npm run build

# Start production server
npm start
```

### Docker Deployment

```dockerfile
# Dockerfile included for containerized deployment
FROM node:18-alpine
WORKDIR /app
COPY package*.json ./
RUN npm ci --only=production
COPY dist ./dist
EXPOSE 3000
CMD ["npm", "start"]
```

### Government Cloud Deployment

For government cloud environments:

1. **AWS GovCloud**: Use provided CloudFormation templates
2. **Azure Government**: Deploy via ARM templates  
3. **Google Cloud for Government**: Use Kubernetes manifests

## 📈 Performance Considerations

### File Processing Limits

- **Maximum file size**: 50MB per file
- **Batch limit**: 100 files per analysis
- **Analysis limit**: 10,000 lines for detailed analysis
- **Memory optimization**: Chunked processing for large files

### Scaling for Government Use

- **Horizontal scaling**: Multiple MCP server instances
- **Load balancing**: Distribute analysis workload
- **Caching**: Results caching for repeated analyses
- **Queue management**: Background processing for large batches

## 🤝 Contributing

This project demonstrates MCP patterns and government development practices. While built as a showcase, contributions that improve the educational value are welcome.

### Development Guidelines

1. **Follow TypeScript best practices**
2. **Maintain government security standards**
3. **Document all public APIs**
4. **Include comprehensive tests**
5. **Follow MCP protocol specifications**

### Reporting Issues

For educational purposes or improvements to the MCP implementation patterns, please open issues with:

- Clear problem description
- Steps to reproduce
- Expected vs actual behavior
- Relevant logs and error messages

## 📄 License

MIT License - See [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- **Anthropic**: For the Model Context Protocol and Claude AI
- **Government Developers**: For inspiring this practical use case
- **Open Source Community**: For the excellent TypeScript and Node.js ecosystem

---

## 📞 Support

This is a demonstration project showcasing MCP integration patterns for government legacy code analysis. For questions about the implementation or to discuss similar use cases:

- **Technical Questions**: Review the code and documentation
- **Government Use Cases**: Consider the patterns and adapt to your needs
- **MCP Implementation**: Refer to the MCP Server patterns demonstrated

**Built with ❤️ for Government Developers and MCP Enthusiasts**
