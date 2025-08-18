/**
 * Core types for the Help Me Modernize MCP server
 * Government legacy code analysis and modernization tool
 */

export interface CodeFile {
  id: string;
  name: string;
  content: string;
  language: CodeLanguage;
  size: number;
  uploadedAt: Date;
}

export type CodeLanguage = 'cobol' | 'java' | 'sql' | 'text' | 'unknown';

export interface AnalysisRequest {
  files: CodeFile[];
  analysisTypes: AnalysisType[];
  outputFormats: OutputFormat[];
  options?: AnalysisOptions;
}

export type AnalysisType = 
  | 'documentation' 
  | 'business-logic' 
  | 'security-vulnerabilities' 
  | 'modernization-suggestions';

export type OutputFormat = 'html' | 'markdown' | 'json' | 'text' | 'pdf';

export interface AnalysisOptions {
  includeCodeExamples?: boolean;
  detailLevel?: 'summary' | 'detailed' | 'comprehensive';
  targetAudience?: 'technical' | 'management' | 'compliance';
  maxAnalysisLines?: number;
}

export interface AnalysisResult {
  id: string;
  fileId: string;
  fileName: string;
  language: CodeLanguage;
  analysisType: AnalysisType;
  results: AnalysisData;
  generatedAt: Date;
  processingTime: number;
}

export interface AnalysisData {
  summary: string;
  findings: Finding[];
  recommendations: Recommendation[];
  codeExamples?: CodeExample[];
  riskLevel?: RiskLevel;
  metadata: AnalysisMetadata;
}

export interface Finding {
  id: string;
  type: string;
  severity: 'low' | 'medium' | 'high' | 'critical';
  title: string;
  description: string;
  location?: CodeLocation;
  impact?: string;
}

export interface Recommendation {
  id: string;
  type: string;
  priority: 'low' | 'medium' | 'high';
  title: string;
  description: string;
  effort: 'low' | 'medium' | 'high';
  benefits: string[];
  implementationNotes?: string;
}

export interface CodeExample {
  id: string;
  title: string;
  legacyCode: string;
  modernCode?: string;
  explanation: string;
  language: string;
}

export interface CodeLocation {
  startLine: number;
  endLine: number;
  startColumn?: number;
  endColumn?: number;
  functionName?: string;
  className?: string;
}

export type RiskLevel = 'low' | 'medium' | 'high' | 'critical';

export interface AnalysisMetadata {
  linesAnalyzed: number;
  functionsFound: number;
  complexity: number;
  maintainabilityIndex?: number;
  estimatedAge?: string;
  patterns: string[];
}

export interface MCPToolRequest {
  name: string;
  arguments: Record<string, unknown>;
}

export interface MCPToolResponse {
  content: Array<{
    type: 'text' | 'image' | 'resource';
    text?: string;
    data?: string;
    mimeType?: string;
  }>;
  isError?: boolean;
}

export interface FileProcessingResult {
  success: boolean;
  fileId?: string;
  fileName: string;
  language: CodeLanguage;
  size: number;
  error?: string;
  warnings?: string[];
}

export interface BatchAnalysisProgress {
  totalFiles: number;
  processedFiles: number;
  currentFile?: string;
  status: 'pending' | 'processing' | 'completed' | 'error';
  startTime: Date;
  estimatedCompletion?: Date;
}

// Government-specific types
export interface ComplianceCheck {
  standard: string; // 'FISMA' | 'FedRAMP' | 'NIST' | 'Custom'
  passed: boolean;
  findings: ComplianceFinding[];
  recommendations: string[];
}

export interface ComplianceFinding {
  ruleId: string;
  severity: 'info' | 'warning' | 'violation';
  description: string;
  location?: CodeLocation;
  remediation: string;
}

export interface GovernmentReport {
  executiveSummary: string;
  technicalFindings: AnalysisResult[];
  complianceChecks: ComplianceCheck[];
  riskAssessment: RiskAssessment;
  recommendedActions: RecommendedAction[];
  generatedAt: Date;
  confidentialityLevel: 'public' | 'internal' | 'confidential' | 'secret';
}

export interface RiskAssessment {
  overallRisk: RiskLevel;
  securityRisk: RiskLevel;
  maintenanceRisk: RiskLevel;
  businessRisk: RiskLevel;
  mitigationStrategies: string[];
}

export interface RecommendedAction {
  priority: number;
  category: string;
  action: string;
  timeline: string;
  effort: string;
  impact: string;
  dependencies: string[];
}
