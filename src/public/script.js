/**
 * Help Me Modernize - Client-side JavaScript
 * Government Legacy Code Analysis Interface
 */

let selectedFiles = [];

// Initialize the application when DOM is loaded
document.addEventListener('DOMContentLoaded', function() {
    initializeFileInput();
});

/**
 * Initialize file input handling
 */
function initializeFileInput() {
    const fileInput = document.getElementById('fileInput');
    if (fileInput) {
        fileInput.addEventListener('change', handleFileSelection);
    }
}

/**
 * Handle file selection with validation
 */
function handleFileSelection(event) {
    selectedFiles = Array.from(event.target.files);
    
    // Client-side validation
    const errors = validateFiles(selectedFiles);
    if (errors.length > 0) {
        alert('Validation errors:\n' + errors.join('\n'));
        event.target.value = ''; // Clear the input
        selectedFiles = [];
    }
    
    updateFileList();
    updateAnalyzeButton();
}

/**
 * Validate selected files against government standards
 */
function validateFiles(files) {
    const maxSize = 50 * 1024 * 1024; // 50MB
    const maxFiles = 100;
    const errors = [];
    
    if (files.length > maxFiles) {
        errors.push(`Maximum ${maxFiles} files allowed (selected: ${files.length})`);
    }
    
    for (let file of files) {
        if (file.size > maxSize) {
            const sizeMB = Math.round(file.size / 1024 / 1024);
            errors.push(`${file.name} exceeds 50MB limit (${sizeMB}MB)`);
        }
        
        // Check for potentially dangerous file names
        if (isDangerousFilename(file.name)) {
            errors.push(`${file.name} contains potentially unsafe characters`);
        }
    }
    
    return errors;
}

/**
 * Check for dangerous filename patterns
 */
function isDangerousFilename(filename) {
    const dangerousPatterns = [
        /\.\./,           // Directory traversal
        /[<>:"|?*]/,      // Windows reserved characters
        /^(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])$/i, // Windows reserved names
    ];
    
    return dangerousPatterns.some(pattern => pattern.test(filename));
}

/**
 * Update the file list display
 */
function updateFileList() {
    const container = document.getElementById('selectedFiles');
    if (!container) return;
    
    if (selectedFiles.length === 0) {
        container.innerHTML = '';
        return;
    }
    
    const totalSize = selectedFiles.reduce((sum, file) => sum + file.size, 0);
    const totalSizeMB = Math.round(totalSize / 1024 / 1024 * 100) / 100;
    
    container.innerHTML = `
        <h4>Selected Files (${selectedFiles.length}, ${totalSizeMB}MB total):</h4>
        ${selectedFiles.map(file => `
            <div style="margin: 5px 0; padding: 5px; background: #e8f4f8; border-radius: 4px;">
                ${file.name} (${Math.round(file.size / 1024)} KB)
            </div>
        `).join('')}
    `;
}

/**
 * Update analyze button state
 */
function updateAnalyzeButton() {
    const btn = document.querySelector('.analyze-btn');
    if (btn) {
        btn.disabled = selectedFiles.length === 0;
    }
}

/**
 * Main analysis function
 */
async function analyzeFiles() {
    if (selectedFiles.length === 0) {
        alert('Please select files to analyze');
        return;
    }
    
    const analysisTypes = getSelectedOptions('analysisType');
    const outputFormats = getSelectedOptions('outputFormat');
    
    if (analysisTypes.length === 0) {
        alert('Please select at least one analysis type');
        return;
    }
    
    if (outputFormats.length === 0) {
        alert('Please select at least one output format');
        return;
    }
    
    // Show loading state
    showLoadingState();
    
    try {
        // Read files with error handling
        const files = await Promise.all(
            selectedFiles.map(async file => {
                try {
                    const content = await readFileAsText(file);
                    return {
                        name: file.name,
                        content: content,
                        language: detectLanguage(file.name)
                    };
                } catch (error) {
                    throw new Error(`Failed to read ${file.name}: ${error.message}`);
                }
            })
        );
        
        // Make API call with timeout
        const controller = new AbortController();
        const timeoutId = setTimeout(() => controller.abort(), 300000); // 5 minute timeout
        
        const response = await fetch('/api/analyze', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({
                files: files,
                analysisTypes: analysisTypes,
                outputFormats: outputFormats,
                options: {
                    detailLevel: 'detailed',
                    targetAudience: 'technical',
                    includeCodeExamples: true
                }
            }),
            signal: controller.signal
        });
        
        clearTimeout(timeoutId);
        
        if (!response.ok) {
            throw new Error(`Server error: ${response.status} ${response.statusText}`);
        }
        
        const result = await response.json();
        
        // Show results
        hideLoadingState();
        
        if (result.success) {
            displayResults(result);
        } else {
            showError(result.error || 'Analysis failed');
        }
        
    } catch (error) {
        hideLoadingState();
        
        if (error.name === 'AbortError') {
            showError('Analysis timed out. Please try with smaller files or fewer analysis types.');
        } else {
            showError(`Analysis failed: ${error.message}`);
        }
    }
}

/**
 * Get selected checkbox values
 */
function getSelectedOptions(name) {
    return Array.from(document.querySelectorAll(`input[name="${name}"]:checked`))
        .map(cb => cb.value);
}

/**
 * Show loading state
 */
function showLoadingState() {
    const resultsSection = document.getElementById('results');
    const loading = document.getElementById('loading');
    const resultsContent = document.getElementById('resultsContent');
    
    if (resultsSection) resultsSection.style.display = 'block';
    if (loading) loading.style.display = 'block';
    if (resultsContent) resultsContent.style.display = 'none';
}

/**
 * Hide loading state
 */
function hideLoadingState() {
    const loading = document.getElementById('loading');
    const resultsContent = document.getElementById('resultsContent');
    
    if (loading) loading.style.display = 'none';
    if (resultsContent) resultsContent.style.display = 'block';
}

/**
 * Show error message
 */
function showError(message) {
    const resultsContent = document.getElementById('resultsContent');
    if (resultsContent) {
        resultsContent.innerHTML = `
            <div style="color: #e74c3c; padding: 20px; border: 1px solid #e74c3c; border-radius: 8px; background: #fdf2f2;">
                <h4>🚨 Analysis Error</h4>
                <p>${escapeHtml(message)}</p>
            </div>
        `;
    }
}

/**
 * Read file as text with proper encoding handling
 */
function readFileAsText(file) {
    return new Promise((resolve, reject) => {
        const reader = new FileReader();
        
        reader.onload = e => {
            try {
                resolve(e.target.result);
            } catch (error) {
                reject(new Error('Failed to read file content'));
            }
        };
        
        reader.onerror = () => {
            reject(new Error('File reading failed'));
        };
        
        reader.readAsText(file, 'UTF-8');
    });
}

/**
 * Detect programming language from filename
 */
function detectLanguage(filename) {
    const ext = filename.toLowerCase().split('.').pop();
    
    // COBOL extensions
    if (['cob', 'cbl', 'cobol', 'cpy', 'copy'].includes(ext)) {
        return 'cobol';
    }
    
    // Java extensions
    if (['java', 'class'].includes(ext)) {
        return 'java';
    }
    
    // SQL extensions
    if (['sql', 'ddl', 'dml', 'proc', 'sp'].includes(ext)) {
        return 'sql';
    }
    
    // Default to text
    return 'text';
}

/**
 * Safe base64 encoding for UTF-8 content
 */
function safeBase64Encode(str) {
    try {
        // Use TextEncoder to convert to UTF-8 bytes
        const encoder = new TextEncoder();
        const bytes = encoder.encode(str);
        
        // Convert bytes to base64
        let binary = '';
        bytes.forEach(byte => {
            binary += String.fromCharCode(byte);
        });
        
        return btoa(binary);
    } catch (error) {
        console.warn('Base64 encoding failed, returning original content:', error);
        return str;
    }
}

/**
 * Display analysis results with proper formatting
 */
function displayResults(result) {
    let html = '<h3>📊 Analysis Results</h3>';
    
    // Summary section
    if (result.summary) {
        html += `
            <div style="background: #e8f5e8; padding: 15px; border-radius: 8px; margin: 15px 0;">
                <h4>📋 Summary</h4>
                <div style="display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 10px;">
                    <p><strong>Files Analyzed:</strong> ${result.summary.filesAnalyzed}</p>
                    <p><strong>Total Findings:</strong> ${result.summary.totalFindings}</p>
                    <p><strong>Total Recommendations:</strong> ${result.summary.totalRecommendations}</p>
                    <p><strong>Average Risk Level:</strong> ${result.summary.averageRiskLevel}</p>
                </div>
            </div>
        `;
    }
    
    // Generated reports section
    if (result.outputs && result.outputs.length > 0) {
        html += '<h4>📄 Generated Reports</h4>';
        result.outputs.forEach(output => {
            // For binary formats like PDF, content is already base64 encoded from server
            const base64Content = (output.format === 'pdf') ? output.content : safeBase64Encode(output.content);
            html += `
                <div style="margin: 10px 0; padding: 15px; background: #f0f8ff; border-radius: 6px; border-left: 4px solid #3498db;">
                    <strong>${output.format.toUpperCase()} Report</strong><br>
                    <small style="color: #666;">Generated at ${new Date().toLocaleString()}</small><br>
                    <a href="data:${output.mimeType};base64,${base64Content}" 
                       download="${output.filename}" 
                       style="color: #3498db; text-decoration: none; font-weight: bold;">
                        📥 Download ${output.filename}
                    </a>
                </div>
            `;
        });
    }
    
    // Detailed findings section
    if (result.results && result.results.length > 0) {
        html += '<h4>🔍 Detailed Analysis Results</h4>';
        result.results.forEach(analysisResult => {
            const typeLabel = analysisResult.analysisType.replace('-', ' ').toUpperCase();
            html += `
                <div style="margin: 15px 0; padding: 15px; background: #f9f9f9; border-radius: 8px; border-left: 4px solid #9b59b6;">
                    <h5>📄 ${escapeHtml(analysisResult.fileName)} - ${typeLabel}</h5>
                    <p style="margin: 10px 0;">${escapeHtml(analysisResult.results.summary || 'Analysis completed')}</p>
                    
                    <div style="display: grid; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); gap: 10px; margin-top: 10px;">
                        ${analysisResult.results.findings ? `<p><strong>🔍 Findings:</strong> ${analysisResult.results.findings.length}</p>` : ''}
                        ${analysisResult.results.recommendations ? `<p><strong>💡 Recommendations:</strong> ${analysisResult.results.recommendations.length}</p>` : ''}
                        <p><strong>⏱️ Processing Time:</strong> ${analysisResult.processingTime}ms</p>
                    </div>
                </div>
            `;
        });
    }
    
    // Error handling section
    if (!result.outputs?.length && !result.results?.length) {
        html += `
            <div style="color: #f39c12; padding: 20px; border: 1px solid #f39c12; border-radius: 8px; background: #fef9e7;">
                <h4>⚠️ No Results Generated</h4>
                <p>The analysis completed but no output was generated. This might be due to:</p>
                <ul style="margin: 10px 0 0 20px;">
                    <li>API key not configured (using mock responses)</li>
                    <li>Selected files contain no analyzable content</li>
                    <li>Analysis type not supported for the selected files</li>
                </ul>
            </div>
        `;
    }
    
    const resultsContent = document.getElementById('resultsContent');
    if (resultsContent) {
        resultsContent.innerHTML = html;
    }
}

/**
 * Escape HTML to prevent XSS
 */
function escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
}
