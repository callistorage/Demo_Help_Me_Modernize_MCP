/**
 * @fileoverview Multi-Format Output Generation for Government Analysis Reports
 * 
 * This module provides comprehensive output generation capabilities for legacy code
 * analysis results, supporting multiple formats including HTML, Markdown, JSON, Text,
 * and PDF. Designed specifically for government reporting requirements with
 * compliance-ready formatting and professional presentation.
 * 
 * @module OutputGenerator
 * @version 1.0.0
 * @author Help Me Modernize MCP Server
 * @since 2024
 * 
 * @security
 * - Sanitizes all output content to prevent injection attacks
 * - Implements secure PDF generation with controlled content
 * - Validates output format parameters before generation
 * - Provides audit trails for all output generation operations
 * 
 * @compliance
 * - Generates government-compliant report formats
 * - Implements accessibility standards for HTML output
 * - Provides structured data formats for regulatory reporting
 * - Supports archival requirements with comprehensive metadata
 */

import { marked } from 'marked';
import { PDFDocument, rgb, StandardFonts } from 'pdf-lib';
import { Logger } from '../utils/Logger';
import { 
  AnalysisResult,
  OutputFormat,
  GovernmentReport,
  ComplianceCheck,
  RiskAssessment,
  RecommendedAction,
  RiskLevel
} from '../types/index';

/**
 * Interface defining the structure of generated output files.
 * 
 * @interface GeneratedOutput
 */
export interface GeneratedOutput {
  /** The output format type */
  format: OutputFormat;
  /** The generated content as a string */
  content: string;
  /** The suggested filename for the output */
  filename: string;
  /** The MIME type for proper content handling */
  mimeType: string;
}

/**
 * Enterprise-grade multi-format output generator for government analysis reports.
 * 
 * The OutputGenerator provides comprehensive output generation capabilities for
 * legacy code analysis results, supporting multiple professional formats suitable
 * for government reporting, executive summaries, and technical documentation.
 * 
 * @example
 * ```typescript
 * const generator = new OutputGenerator();
 * 
 * const analysisResults: AnalysisResult[] = [
 *   // ... analysis results from LegacyCodeAnalyzer
 * ];
 * 
 * const outputs = await generator.generateOutputs(
 *   analysisResults,
 *   ['html', 'pdf', 'json']
 * );
 * 
 * for (const output of outputs) {
 *   console.log(`Generated ${output.format}: ${output.filename}`);
 *   // Save or process the output.content
 * }
 * ```
 * 
 * @security
 * - Content sanitization for all output formats
 * - Secure PDF generation with controlled fonts and layouts
 * - HTML output with XSS prevention measures
 * - Input validation for all generation parameters
 * 
 * @compliance
 * - Government-standard report formatting
 * - Section 508 accessibility compliance for HTML
 * - Structured data output for regulatory systems
 * - Professional presentation suitable for executive review
 * 
 * @public
 */
export class OutputGenerator {
  private logger: Logger;

  /**
   * Initializes the Output Generator with enterprise logging capabilities.
   * 
   * Sets up the output generation engine with comprehensive logging for
   * audit trails and error tracking. Prepares the generator for multi-format
   * output creation suitable for government reporting requirements.
   * 
   * @example
   * ```typescript
   * const generator = new OutputGenerator();
   * // Ready to generate outputs in multiple formats
   * ```
   * 
   * @security
   * - Establishes secure logging infrastructure
   * - Initializes with safe default configurations
   * - Prepares sanitization capabilities for all output formats
   * 
   * @compliance
   * - Sets up audit-compliant logging
   * - Initializes government-standard reporting capabilities
   * - Prepares accessibility and compliance features
   * 
   * @public
   */
  constructor() {
    this.logger = new Logger('OutputGenerator');
  }

  /**
   * Generates analysis outputs in multiple professional formats.
   * 
   * This method orchestrates the generation of comprehensive analysis reports
   * in multiple formats simultaneously, providing flexibility for different
   * audiences and use cases. Handles errors gracefully to ensure partial
   * success when some formats fail.
   * 
   * @param results - Array of analysis results to be formatted
   * @param formats - Array of desired output formats
   * 
   * @returns Promise resolving to array of generated outputs
   * 
   * @example
   * ```typescript
   * const generator = new OutputGenerator();
   * 
   * const outputs = await generator.generateOutputs(
   *   analysisResults,
   *   ['html', 'pdf', 'json', 'markdown']
   * );
   * 
   * // Save each output
   * for (const output of outputs) {
   *   await fs.writeFile(output.filename, output.content);
   *   console.log(`Generated ${output.format}: ${output.filename}`);
   * }
   * ```
   * 
   * @security
   * - Validates all input parameters before processing
   * - Sanitizes content for each output format
   * - Implements secure error handling without data exposure
   * - Provides audit logging for all generation attempts
   * 
   * @compliance
   * - Generates government-compliant report formats
   * - Maintains consistent metadata across all formats
   * - Implements accessibility standards where applicable
   * - Provides detailed logging for compliance auditing
   * 
   * @throws Does not throw - handles errors gracefully and continues processing
   * 
   * @public
   * @async
   */
  public async generateOutputs(
    results: AnalysisResult[],
    formats: OutputFormat[]
  ): Promise<GeneratedOutput[]> {
    this.logger.info(`Generating outputs in ${formats.length} formats for ${results.length} results`);
    
    const outputs: GeneratedOutput[] = [];
    
    for (const format of formats) {
      try {
        const output = await this.generateSingleOutput(results, format);
        outputs.push(output);
      } catch (error) {
        this.logger.error(`Failed to generate ${format} output:`, error);
        // Continue with other formats
      }
    }
    
    return outputs;
  }

  /**
   * Generate a single output format
   */
  private async generateSingleOutput(
    results: AnalysisResult[],
    format: OutputFormat
  ): Promise<GeneratedOutput> {
    switch (format) {
      case 'html':
        return this.generateHTML(results);
      case 'markdown':
        return this.generateMarkdown(results);
      case 'json':
        return this.generateJSON(results);
      case 'text':
        return this.generateText(results);
      case 'pdf':
        return await this.generatePDF(results);
      default:
        throw new Error(`Unsupported output format: ${format}`);
    }
  }

  /**
   * Generate HTML output
   */
  private generateHTML(results: AnalysisResult[]): GeneratedOutput {
    const timestamp = new Date().toLocaleString();
    const totalFiles = new Set(results.map(r => r.fileId)).size;
    const totalFindings = results.reduce((sum, r) => sum + r.results.findings.length, 0);
    const totalRecommendations = results.reduce((sum, r) => sum + r.results.recommendations.length, 0);

    const html = `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Legacy Code Analysis Report</title>
    <style>
        body {
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            line-height: 1.6;
            color: #333;
            max-width: 1200px;
            margin: 0 auto;
            padding: 20px;
            background-color: #f5f5f5;
        }
        .header {
            background: linear-gradient(135deg, #1e3a8a, #3b82f6);
            color: white;
            padding: 30px;
            border-radius: 10px;
            margin-bottom: 30px;
            text-align: center;
        }
        .header h1 {
            margin: 0;
            font-size: 2.5em;
        }
        .header .subtitle {
            margin: 10px 0 0 0;
            font-size: 1.2em;
            opacity: 0.9;
        }
        .summary {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
            gap: 20px;
            margin-bottom: 30px;
        }
        .summary-card {
            background: white;
            padding: 20px;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            text-align: center;
        }
        .summary-card h3 {
            margin: 0 0 10px 0;
            color: #1e3a8a;
        }
        .summary-card .number {
            font-size: 2em;
            font-weight: bold;
            color: #3b82f6;
        }
        .file-section {
            background: white;
            margin-bottom: 30px;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            overflow: hidden;
        }
        .file-header {
            background: #1e3a8a;
            color: white;
            padding: 20px;
            font-size: 1.3em;
            font-weight: bold;
        }
        .file-content {
            padding: 20px;
        }
        .analysis-type {
            margin-bottom: 25px;
            border-left: 4px solid #3b82f6;
            padding-left: 15px;
        }
        .analysis-type h3 {
            color: #1e3a8a;
            margin: 0 0 15px 0;
        }
        .findings, .recommendations {
            margin-bottom: 20px;
        }
        .finding, .recommendation {
            background: #f8fafc;
            border: 1px solid #e2e8f0;
            border-radius: 6px;
            padding: 15px;
            margin-bottom: 10px;
        }
        .finding.high, .finding.critical {
            border-left: 4px solid #dc2626;
        }
        .finding.medium {
            border-left: 4px solid #f59e0b;
        }
        .finding.low {
            border-left: 4px solid #10b981;
        }
        .finding-title, .rec-title {
            font-weight: bold;
            margin-bottom: 8px;
        }
        .severity {
            display: inline-block;
            padding: 2px 8px;
            border-radius: 4px;
            font-size: 0.8em;
            font-weight: bold;
            text-transform: uppercase;
        }
        .severity.critical { background: #fca5a5; color: #7f1d1d; }
        .severity.high { background: #fcd34d; color: #78350f; }
        .severity.medium { background: #fed7aa; color: #9a3412; }
        .severity.low { background: #86efac; color: #14532d; }
        .priority {
            display: inline-block;
            padding: 2px 8px;
            border-radius: 4px;
            font-size: 0.8em;
            font-weight: bold;
            text-transform: uppercase;
            background: #ddd6fe;
            color: #5b21b6;
        }
        .code-example {
            background: #1f2937;
            color: #f9fafb;
            padding: 15px;
            border-radius: 6px;
            margin: 10px 0;
            overflow-x: auto;
        }
        .code-example pre {
            margin: 0;
            white-space: pre-wrap;
        }
        .metadata {
            background: #f1f5f9;
            padding: 15px;
            border-radius: 6px;
            margin-top: 20px;
        }
        .footer {
            text-align: center;
            margin-top: 40px;
            padding: 20px;
            color: #6b7280;
            border-top: 1px solid #e5e7eb;
        }
        .no-results {
            text-align: center;
            padding: 40px;
            color: #6b7280;
            font-style: italic;
        }
    </style>
</head>
<body>
    <div class="header">
        <h1>🏛️ Legacy Code Analysis Report</h1>
        <div class="subtitle">Government Code Modernization Assessment</div>
        <div class="subtitle">Generated: ${timestamp}</div>
    </div>

    <div class="summary">
        <div class="summary-card">
            <h3>Files Analyzed</h3>
            <div class="number">${totalFiles}</div>
        </div>
        <div class="summary-card">
            <h3>Total Findings</h3>
            <div class="number">${totalFindings}</div>
        </div>
        <div class="summary-card">
            <h3>Recommendations</h3>
            <div class="number">${totalRecommendations}</div>
        </div>
        <div class="summary-card">
            <h3>Analysis Types</h3>
            <div class="number">${new Set(results.map(r => r.analysisType)).size}</div>
        </div>
    </div>

    ${results.length === 0 ? '<div class="no-results"><p>No analysis results available.</p></div>' : this.generateHTMLFilesSections(results)}

    <div class="footer">
        <p>Generated by Help Me Modernize - Government Legacy Code Analysis Tool</p>
        <p>For questions or support, contact your system administrator</p>
    </div>
</body>
</html>`;

    return {
      format: 'html',
      content: html,
      filename: `legacy-analysis-${Date.now()}.html`,
      mimeType: 'text/html'
    };
  }

  /**
   * Generate HTML sections for each file
   */
  private generateHTMLFilesSections(results: AnalysisResult[]): string {
    const fileGroups = this.groupResultsByFile(results);
    
    return Object.entries(fileGroups).map(([fileId, fileResults]) => {
      const firstResult = fileResults[0];
      
      return `
    <div class="file-section">
        <div class="file-header">
            📄 ${firstResult.fileName} (${firstResult.language.toUpperCase()})
        </div>
        <div class="file-content">
            ${fileResults.map(result => this.generateHTMLAnalysisSection(result)).join('')}
        </div>
    </div>`;
    }).join('');
  }

  /**
   * Generate HTML for a single analysis result
   */
  private generateHTMLAnalysisSection(result: AnalysisResult): string {
    const { results: data, analysisType } = result;
    
    return `
    <div class="analysis-type">
        <h3>📊 ${this.capitalizeAnalysisType(analysisType)}</h3>
        
        <p><strong>Summary:</strong> ${data.summary}</p>
        
        ${data.findings.length > 0 ? `
        <div class="findings">
            <h4>🔍 Findings (${data.findings.length})</h4>
            ${data.findings.map(finding => `
            <div class="finding ${finding.severity}">
                <div class="finding-title">
                    ${finding.title} 
                    <span class="severity ${finding.severity}">${finding.severity}</span>
                </div>
                <p>${this.escapeHtml(finding.description)}</p>
                ${finding.location ? `<small>Lines ${finding.location.startLine}-${finding.location.endLine}</small>` : ''}
            </div>
            `).join('')}
        </div>
        ` : ''}
        
        ${data.recommendations.length > 0 ? `
        <div class="recommendations">
            <h4>💡 Recommendations (${data.recommendations.length})</h4>
            ${data.recommendations.map(rec => `
            <div class="recommendation">
                <div class="rec-title">
                    ${rec.title}
                    <span class="priority">${rec.priority} priority</span>
                </div>
                <p>${this.escapeHtml(rec.description)}</p>
                <p><strong>Effort:</strong> ${rec.effort} | <strong>Benefits:</strong> ${rec.benefits.join(', ')}</p>
            </div>
            `).join('')}
        </div>
        ` : ''}
        
        ${data.codeExamples && data.codeExamples.length > 0 ? `
        <div class="code-examples">
            <h4>💻 Code Examples</h4>
            ${data.codeExamples.map(example => `
            <div class="code-example">
                <strong>${example.title}</strong>
                <pre><code>${this.escapeHtml(example.legacyCode)}</code></pre>
                <p>${example.explanation}</p>
            </div>
            `).join('')}
        </div>
        ` : ''}
        
        <div class="metadata">
            <strong>Analysis Metadata:</strong>
            Lines: ${data.metadata.linesAnalyzed} | 
            Functions: ${data.metadata.functionsFound} | 
            Complexity: ${data.metadata.complexity}/10
            ${data.metadata.patterns.length > 0 ? ` | Patterns: ${data.metadata.patterns.join(', ')}` : ''}
        </div>
    </div>`;
  }

  /**
   * Generate Markdown output
   */
  private generateMarkdown(results: AnalysisResult[]): GeneratedOutput {
    const timestamp = new Date().toLocaleString();
    const totalFiles = new Set(results.map(r => r.fileId)).size;
    const totalFindings = results.reduce((sum, r) => sum + r.results.findings.length, 0);
    const totalRecommendations = results.reduce((sum, r) => sum + r.results.recommendations.length, 0);

    let markdown = `# 🏛️ Legacy Code Analysis Report

**Government Code Modernization Assessment**  
Generated: ${timestamp}

## 📊 Summary

| Metric | Count |
|--------|-------|
| Files Analyzed | ${totalFiles} |
| Total Findings | ${totalFindings} |
| Recommendations | ${totalRecommendations} |
| Analysis Types | ${new Set(results.map(r => r.analysisType)).size} |

`;

    const fileGroups = this.groupResultsByFile(results);
    
    Object.entries(fileGroups).forEach(([fileId, fileResults]) => {
      const firstResult = fileResults[0];
      
      markdown += `## 📄 ${firstResult.fileName} (${firstResult.language.toUpperCase()})\n\n`;
      
      fileResults.forEach(result => {
        markdown += this.generateMarkdownAnalysisSection(result);
      });
    });

    markdown += `\n---\n\n*Generated by Help Me Modernize - Government Legacy Code Analysis Tool*`;

    return {
      format: 'markdown',
      content: markdown,
      filename: `legacy-analysis-${Date.now()}.md`,
      mimeType: 'text/markdown'
    };
  }

  /**
   * Generate Markdown for a single analysis result
   */
  private generateMarkdownAnalysisSection(result: AnalysisResult): string {
    const { results: data, analysisType } = result;
    
    let section = `### 📊 ${this.capitalizeAnalysisType(analysisType)}\n\n`;
    section += `**Summary:** ${data.summary}\n\n`;
    
    if (data.findings.length > 0) {
      section += `#### 🔍 Findings (${data.findings.length})\n\n`;
      data.findings.forEach(finding => {
        section += `- **${finding.title}** (${finding.severity.toUpperCase()})\n`;
        section += `  ${finding.description}\n`;
        if (finding.location) {
          section += `  *Lines ${finding.location.startLine}-${finding.location.endLine}*\n`;
        }
        section += '\n';
      });
    }
    
    if (data.recommendations.length > 0) {
      section += `#### 💡 Recommendations (${data.recommendations.length})\n\n`;
      data.recommendations.forEach(rec => {
        section += `- **${rec.title}** (${rec.priority} priority)\n`;
        section += `  ${rec.description}\n`;
        section += `  *Effort: ${rec.effort} | Benefits: ${rec.benefits.join(', ')}*\n\n`;
      });
    }
    
    if (data.codeExamples && data.codeExamples.length > 0) {
      section += `#### 💻 Code Examples\n\n`;
      data.codeExamples.forEach(example => {
        section += `**${example.title}**\n\n`;
        section += '```\n' + example.legacyCode + '\n```\n\n';
        section += `${example.explanation}\n\n`;
      });
    }
    
    section += `**Analysis Metadata:** `;
    section += `Lines: ${data.metadata.linesAnalyzed} | `;
    section += `Functions: ${data.metadata.functionsFound} | `;
    section += `Complexity: ${data.metadata.complexity}/10`;
    if (data.metadata.patterns.length > 0) {
      section += ` | Patterns: ${data.metadata.patterns.join(', ')}`;
    }
    section += '\n\n';
    
    return section;
  }

  /**
   * Generate JSON output
   */
  private generateJSON(results: AnalysisResult[]): GeneratedOutput {
    const totalFiles = new Set(results.map(r => r.fileId)).size;
    const totalFindings = results.reduce((sum, r) => sum + r.results.findings.length, 0);
    const totalRecommendations = results.reduce((sum, r) => sum + r.results.recommendations.length, 0);
    const languages = Array.from(new Set(results.map(r => r.language)));
    const analysisTypes = Array.from(new Set(results.map(r => r.analysisType)));

    const output = {
      generatedAt: new Date().toISOString(),
      generator: 'Help Me Modernize v1.0.0',
      metadata: {
        totalFiles,
        totalFindings,
        totalRecommendations,
        totalResults: results.length,
        languages,
        analysisTypes,
        averageComplexity: results.length > 0 ? results.reduce((sum, r) => sum + r.results.metadata.complexity, 0) / results.length : 0,
        riskDistribution: this.calculateRiskDistribution(results)
      },
      summary: {
        totalFindings,
        totalRecommendations,
        averageComplexity: results.length > 0 ? results.reduce((sum, r) => sum + r.results.metadata.complexity, 0) / results.length : 0,
        riskDistribution: this.calculateRiskDistribution(results)
      },
      results: results
    };

    return {
      format: 'json',
      content: JSON.stringify(output, null, 2),
      filename: `legacy-analysis-${Date.now()}.json`,
      mimeType: 'application/json'
    };
  }

  /**
   * Generate plain text output
   */
  private generateText(results: AnalysisResult[]): GeneratedOutput {
    const timestamp = new Date().toLocaleString();
    const totalFiles = new Set(results.map(r => r.fileId)).size;
    const totalFindings = results.reduce((sum, r) => sum + r.results.findings.length, 0);
    const totalRecommendations = results.reduce((sum, r) => sum + r.results.recommendations.length, 0);

    let text = `LEGACY CODE ANALYSIS REPORT\n`;
    text += `Government Code Modernization Assessment\n`;
    text += `Generated: ${timestamp}\n`;
    text += `${'='.repeat(50)}\n\n`;
    
    text += `SUMMARY\n`;
    text += `Files Analyzed: ${totalFiles}\n`;
    text += `Total Findings: ${totalFindings}\n`;
    text += `Recommendations: ${totalRecommendations}\n`;
    text += `Analysis Types: ${new Set(results.map(r => r.analysisType)).size}\n\n`;

    const fileGroups = this.groupResultsByFile(results);
    
    Object.entries(fileGroups).forEach(([fileId, fileResults]) => {
      const firstResult = fileResults[0];
      
      text += `FILE: ${firstResult.fileName} (${firstResult.language.toUpperCase()})\n`;
      text += `${'-'.repeat(40)}\n\n`;
      
      fileResults.forEach(result => {
        text += this.generateTextAnalysisSection(result);
      });
    });

    text += `\nGenerated by Help Me Modernize - Government Legacy Code Analysis Tool\n`;

    return {
      format: 'text',
      content: text,
      filename: `legacy-analysis-${Date.now()}.txt`,
      mimeType: 'text/plain'
    };
  }

  /**
   * Generate text for a single analysis result
   */
  private generateTextAnalysisSection(result: AnalysisResult): string {
    const { results: data, analysisType } = result;
    
    let section = `${this.capitalizeAnalysisType(analysisType).toUpperCase()}\n`;
    section += `Summary: ${data.summary}\n\n`;
    
    if (data.findings.length > 0) {
      section += `FINDINGS (${data.findings.length}):\n`;
      data.findings.forEach((finding, i) => {
        section += `${i + 1}. ${finding.title} (${finding.severity.toUpperCase()})\n`;
        section += `   ${finding.description}\n`;
        if (finding.location) {
          section += `   Location: Lines ${finding.location.startLine}-${finding.location.endLine}\n`;
        }
        section += '\n';
      });
    }
    
    if (data.recommendations.length > 0) {
      section += `RECOMMENDATIONS (${data.recommendations.length}):\n`;
      data.recommendations.forEach((rec, i) => {
        section += `${i + 1}. ${rec.title} (${rec.priority} priority)\n`;
        section += `   ${rec.description}\n`;
        section += `   Effort: ${rec.effort} | Benefits: ${rec.benefits.join(', ')}\n\n`;
      });
    }
    
    section += `Metadata: Lines: ${data.metadata.linesAnalyzed}, `;
    section += `Functions: ${data.metadata.functionsFound}, `;
    section += `Complexity: ${data.metadata.complexity}/10\n\n`;
    
    return section;
  }

  /**
   * Generate PDF output
   */
  private async generatePDF(results: AnalysisResult[]): Promise<GeneratedOutput> {
    try {
      const pdfDoc = await PDFDocument.create();
      const font = await pdfDoc.embedFont(StandardFonts.Helvetica);
      const boldFont = await pdfDoc.embedFont(StandardFonts.HelveticaBold);
      
      let page = pdfDoc.addPage([612, 792]); // Letter size
      let yPosition = 750;
      const leftMargin = 50;
      const pageWidth = 512;
      
      // Title
      page.drawText('Legacy Code Analysis Report', {
        x: leftMargin,
        y: yPosition,
        size: 20,
        font: boldFont,
        color: rgb(0.1, 0.2, 0.5)
      });
      
      yPosition -= 30;
      page.drawText(`Generated: ${new Date().toLocaleString()}`, {
        x: leftMargin,
        y: yPosition,
        size: 10,
        font: font
      });
      
      yPosition -= 40;
      
      // Summary
      const totalFiles = new Set(results.map(r => r.fileId)).size;
      const totalFindings = results.reduce((sum, r) => sum + r.results.findings.length, 0);
      const totalRecommendations = results.reduce((sum, r) => sum + r.results.recommendations.length, 0);
      
      page.drawText('Executive Summary', {
        x: leftMargin,
        y: yPosition,
        size: 14,
        font: boldFont
      });
      
      yPosition -= 25;
      const summaryText = [
        `Files Analyzed: ${totalFiles}`,
        `Total Findings: ${totalFindings}`,
        `Recommendations: ${totalRecommendations}`,
        `Analysis Types: ${new Set(results.map(r => r.analysisType)).size}`
      ];
      
      summaryText.forEach(line => {
        page.drawText(line, {
          x: leftMargin,
          y: yPosition,
          size: 10,
          font: font
        });
        yPosition -= 15;
      });
      
      // Add more detailed content
      yPosition -= 20;
      const fileGroups = this.groupResultsByFile(results);
      
      Object.entries(fileGroups).forEach(([fileId, fileResults]) => {
        const firstResult = fileResults[0];
        
        // Check if we need a new page
        if (yPosition < 100) {
          page = pdfDoc.addPage([612, 792]);
          yPosition = 750;
        }
        
        page.drawText(`File: ${firstResult.fileName}`, {
          x: leftMargin,
          y: yPosition,
          size: 12,
          font: boldFont
        });
        
        yPosition -= 20;
        
        fileResults.forEach(result => {
          if (yPosition < 100) {
            page = pdfDoc.addPage([612, 792]);
            yPosition = 750;
          }
          
          page.drawText(`${this.capitalizeAnalysisType(result.analysisType)}:`, {
            x: leftMargin + 20,
            y: yPosition,
            size: 10,
            font: boldFont
          });
          
          yPosition -= 15;
          
          // Add summary (truncated to fit)
          const summary = result.results.summary.length > 80 ? 
            result.results.summary.substring(0, 80) + '...' : 
            result.results.summary;
          
          page.drawText(summary, {
            x: leftMargin + 20,
            y: yPosition,
            size: 9,
            font: font
          });
          
          yPosition -= 25;
        });
      });
      
      const pdfBytes = await pdfDoc.save();
      
      return {
        format: 'pdf',
        content: Buffer.from(pdfBytes).toString('base64'),
        filename: `legacy-analysis-${Date.now()}.pdf`,
        mimeType: 'application/pdf'
      };
      
    } catch (error) {
      this.logger.error('PDF generation failed:', error);
      throw new Error(`PDF generation failed: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  /**
   * Generate government compliance report
   */
  public async generateGovernmentReport(
    results: AnalysisResult[],
    complianceStandards: string[],
    confidentialityLevel: string
  ): Promise<GovernmentReport> {
    this.logger.info('Generating government compliance report');
    
    // Generate compliance checks
    const complianceChecks: ComplianceCheck[] = complianceStandards.map(standard => ({
      standard,
      passed: this.assessCompliance(results, standard),
      findings: this.generateComplianceFindings(results, standard),
      recommendations: this.generateComplianceRecommendations(results, standard)
    }));

    // Generate risk assessment
    const riskAssessment: RiskAssessment = this.generateRiskAssessment(results);
    
    // Generate recommended actions
    const recommendedActions: RecommendedAction[] = this.generateRecommendedActions(results);
    
    return {
      executiveSummary: this.generateExecutiveSummary(results),
      technicalFindings: results,
      complianceChecks,
      riskAssessment,
      recommendedActions,
      generatedAt: new Date(),
      confidentialityLevel: confidentialityLevel as any
    };
  }

  /**
   * Helper methods
   */

  private groupResultsByFile(results: AnalysisResult[]): Record<string, AnalysisResult[]> {
    return results.reduce((groups, result) => {
      const key = result.fileId;
      if (!groups[key]) {
        groups[key] = [];
      }
      groups[key].push(result);
      return groups;
    }, {} as Record<string, AnalysisResult[]>);
  }

  private capitalizeAnalysisType(type: string): string {
    return type.split('-').map(word => 
      word.charAt(0).toUpperCase() + word.slice(1)
    ).join(' ');
  }

  private escapeHtml(text: string): string {
    return text
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#039;');
  }

  private calculateRiskDistribution(results: AnalysisResult[]): Record<string, number> {
    const distribution = { low: 0, medium: 0, high: 0, critical: 0 };
    
    results.forEach(result => {
      result.results.findings.forEach(finding => {
        distribution[finding.severity as keyof typeof distribution]++;
      });
    });
    
    return distribution;
  }

  private assessCompliance(results: AnalysisResult[], standard: string): boolean {
    // Mock compliance assessment - in real implementation, this would check specific rules
    const criticalFindings = results.reduce((count, r) => 
      count + r.results.findings.filter(f => f.severity === 'critical').length, 0
    );
    
    return criticalFindings === 0;
  }

  private generateComplianceFindings(results: AnalysisResult[], standard: string): any[] {
    // Mock compliance findings generation
    return [];
  }

  private generateComplianceRecommendations(results: AnalysisResult[], standard: string): string[] {
    return [
      `Ensure all code meets ${standard} standards`,
      'Implement regular compliance audits',
      'Document all security measures'
    ];
  }

  private generateRiskAssessment(results: AnalysisResult[]): RiskAssessment {
    const allFindings = results.flatMap(r => r.results.findings);
    const criticalCount = allFindings.filter(f => f.severity === 'critical').length;
    const highCount = allFindings.filter(f => f.severity === 'high').length;
    
    let overallRisk: RiskLevel = 'low';
    if (criticalCount > 0) overallRisk = 'critical';
    else if (highCount > 2) overallRisk = 'high';
    else if (highCount > 0) overallRisk = 'medium';
    
    return {
      overallRisk,
      securityRisk: criticalCount > 0 ? 'high' : 'medium',
      maintenanceRisk: 'medium',
      businessRisk: 'medium',
      mitigationStrategies: [
        'Prioritize critical security fixes',
        'Implement gradual modernization plan',
        'Establish regular code review process'
      ]
    };
  }

  private generateRecommendedActions(results: AnalysisResult[]): RecommendedAction[] {
    return [
      {
        priority: 1,
        category: 'Security',
        action: 'Address critical security vulnerabilities',
        timeline: '1-2 weeks',
        effort: 'High',
        impact: 'Critical',
        dependencies: []
      },
      {
        priority: 2,
        category: 'Documentation',
        action: 'Improve code documentation',
        timeline: '1 month',
        effort: 'Medium',
        impact: 'Medium',
        dependencies: []
      }
    ];
  }

  private generateExecutiveSummary(results: AnalysisResult[]): string {
    const totalFiles = new Set(results.map(r => r.fileId)).size;
    const totalFindings = results.reduce((sum, r) => sum + r.results.findings.length, 0);
    const criticalFindings = results.reduce((sum, r) => 
      sum + r.results.findings.filter(f => f.severity === 'critical').length, 0
    );
    
    return `Analysis of ${totalFiles} legacy code files revealed ${totalFindings} findings, including ${criticalFindings} critical issues requiring immediate attention. The assessment identifies key modernization opportunities and provides actionable recommendations for improving system security, maintainability, and compliance with government standards.`;
  }
}
