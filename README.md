# 🏛️ Help Me Modernize

**Enterprise MCP-Powered Legacy Code Modernization Tool for Government Systems**

[![TypeScript](https://img.shields.io/badge/TypeScript-5.2-blue.svg)](https://www.typescriptlang.org/)
[![MCP](https://img.shields.io/badge/MCP-Model%20Context%20Protocol-green.svg)](https://modelcontextprotocol.io/)
[![Tests](https://img.shields.io/badge/Tests-65%20Passing-brightgreen.svg)](https://github.com)
[![Status](https://img.shields.io/badge/Status-Production%20Ready-success.svg)](https://github.com)

Enterprise-grade TypeScript application using Model Context Protocol (MCP) to analyze, document, and modernize legacy government code systems (COBOL, Java, SQL). Built with government compliance standards and air-gapped deployment support.

## 🎯 Features

- **🤖 AI Analysis**: Claude 4 integration with offline fallback
- **📊 Professional Reports**: HTML, PDF, JSON, Markdown outputs  
- **🛡️ Government Ready**: FISMA/FedRAMP compliance, audit trails
- **🏗️ MCP Architecture**: Advanced integration patterns and tools
- **🧪 Enterprise Quality**: 65 tests, 80% coverage, strict TypeScript

**Use Cases**: Documentation generation, security assessment, knowledge transfer, modernization planning

## 🚀 Quick Start

```bash
git clone <repository-url>
cd help-me-modernize
npm install && npm run build

# Optional: Add Anthropic API key to .env
echo "ANTHROPIC_API_KEY=your_key" > .env

# Web Interface
npm run start:web

# MCP Server  
npm start
```
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
## 🏗️ Architecture

**MCP Server as Primary Application** - business logic in MCP server, web interface as thin client.

```
Web Client ◄──► MCP Server ◄──► Claude API
```

**Key Components**: MCP tools, AI analysis engine, multi-format outputs, enterprise utilities, comprehensive tests (65 tests, 80% coverage)

## 💻 Usage

**Web Interface**: Upload files → Select analysis types → Download reports

**API**: 
```bash
POST /api/analyze
{"files": [...], "analysisTypes": ["documentation", "security"], "outputFormats": ["html", "pdf"]}
```

**Analysis Types**: Documentation, Security Assessment, Modernization Planning, Business Logic
**Output Formats**: HTML, PDF, JSON, Markdown

## 🛡️ Security & Government Compliance

- **Standards**: FISMA, FedRAMP, NIST compliance ready
- **Air-Gapped**: Offline deployment with local AI fallbacks
- **Security**: Input validation, audit logging, secure file handling
- **Enterprise**: Type safety, structured error handling, comprehensive testing
npm run test:coverage
```

### 🏆 Enterprise Development Features

**Advanced TypeScript Configuration**:
```bash
# Strict compilation with 20+ enterprise rules
npm run typecheck

# Watch mode for development
npm run build:watch

# Clean build artifacts
npm run clean
```

**Testing Infrastructure**:
```bash
# Run all 65 tests
npm test

# Run specific test categories
npm test -- --grep "unit"
## 🧪 Development

### Testing
```bash
npm test              # Run all 65 tests (80% coverage)
npm run test:coverage # Generate coverage reports
npm run test:watch    # Watch mode for development
```

### Code Quality
```bash
npm run lint          # ESLint with 120+ rules
npm run typecheck     # TypeScript validation
npm audit             # Security vulnerability scan
```

## 📚 MCP Integration

This project demonstrates **MCP Server as Primary Application** pattern with government-specific tools:

- `analyze_legacy_code`: Core analysis functionality
- `upload_file`: Secure file processing  
- `generate_government_report`: Compliance reporting
- `get_analysis_status`: Progress tracking

### Architecture Benefits
- Business logic centralized in MCP server
- Thin clients via MCP protocol
- Extensible for multiple frontends
- Multi-model AI integration ready

## 🤝 Contributing

1. Fork the repository
2. Create feature branch (`git checkout -b feature/amazing-feature`)
3. Run tests (`npm test`)
4. Commit changes (`git commit -m 'Add amazing feature'`)
5. Push to branch (`git push origin feature/amazing-feature`)
6. Open Pull Request

## � License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- Built with [Model Context Protocol](https://modelcontextprotocol.io/) by Anthropic
- Powered by [Claude 4](https://www.anthropic.com/) for advanced AI analysis
- TypeScript and Node.js ecosystem
- Government developers and legacy system maintainers

---

**Help Me Modernize** - Enterprise MCP patterns for government legacy code modernization 🏛️
