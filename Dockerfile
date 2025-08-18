# Help Me Modernize - Government Legacy Code Analysis Tool
# Multi-stage Docker build for production deployment

# Build stage
FROM node:18-alpine AS builder

# Set working directory
WORKDIR /app

# Copy package files
COPY package*.json ./

# Install dependencies
RUN npm ci --only=production && npm cache clean --force

# Copy source code
COPY . .

# Build the application
RUN npm run build

# Production stage
FROM node:18-alpine AS production

# Add security: Create non-root user for government compliance
RUN addgroup -g 1001 -S nodejs && \
    adduser -S helpmemodernize -u 1001

# Set working directory
WORKDIR /app

# Copy package files
COPY package*.json ./

# Install only production dependencies
RUN npm ci --only=production && npm cache clean --force

# Copy built application from builder stage
COPY --from=builder /app/dist ./dist
COPY --from=builder /app/README.md ./

# Create logs directory
RUN mkdir -p logs && chown -R helpmemodernize:nodejs logs

# Set security: Change ownership of app directory
RUN chown -R helpmemodernize:nodejs /app

# Switch to non-root user
USER helpmemodernize

# Expose port
EXPOSE 3000

# Add health check for government compliance monitoring
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD node -e "require('http').get('http://localhost:3000/health', (res) => { process.exit(res.statusCode === 200 ? 0 : 1) })"

# Set environment variables
ENV NODE_ENV=production
ENV PORT=3000

# Start the application
CMD ["npm", "start"]

# Metadata labels for government compliance
LABEL maintainer="Help Me Modernize Team"
LABEL version="1.0.0"
LABEL description="Government legacy code analysis and modernization tool"
LABEL org.opencontainers.image.title="Help Me Modernize"
LABEL org.opencontainers.image.description="MCP-powered legacy code documentation tool for government developers"
LABEL org.opencontainers.image.version="1.0.0"
LABEL org.opencontainers.image.vendor="Government DevRel Showcase"
LABEL compliance.fisma="placeholder"
LABEL compliance.fedramp="placeholder"
LABEL security.classification="unclassified"
