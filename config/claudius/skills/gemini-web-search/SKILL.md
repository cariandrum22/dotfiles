---
name: gemini-web-search
description: Execute web searches using Gemini CLI instead of built-in web search
---

# Gemini Web Search

This command performs web searches using the Gemini CLI, which provides more accurate and up-to-date results than the built-in web search tool.

## Prerequisites

Before using this command:
1. Ensure Gemini CLI is installed and configured
2. Verify API credentials are properly set
3. Check network connectivity

## Search Process

I will:
- **Format your query** for optimal search results
- **Execute search** via Gemini CLI command
- **Parse results** to extract relevant information
- **Present findings** in a structured format
- **Cite sources** with proper attribution

## Query Format

The command uses the following format:
```bash
gemini --prompt "WebSearch: <your query>"
```

### Query Types
- **Technical documentation**: Programming languages, frameworks, libraries
- **Current events**: Recent news, updates, announcements
- **Research topics**: Academic papers, technical specifications
- **Product information**: Software releases, version comparisons
- **Troubleshooting**: Error messages, bug reports, solutions

### Query Optimization
- Use specific technical terms
- Include version numbers when relevant
- Add date constraints for time-sensitive information
- Specify programming language or technology stack

## Usage Instructions

1. **Provide your search query** with clear intent

2. **Specify any constraints**:
   - Time range (e.g., "past year", "since 2024")
   - Source preferences (e.g., "official docs only")
   - Language or region

3. **Review the results** and request refinement if needed

## Example Queries

For technical documentation:
```
Search for: React 18 concurrent features documentation
Executed: gemini --prompt "WebSearch: React 18 concurrent features official documentation"
```

For troubleshooting:
```
Search for: TypeScript error TS2322 type not assignable
Executed: gemini --prompt "WebSearch: TypeScript error TS2322 type not assignable solutions"
```

For current information:
```
Search for: Node.js latest LTS version 2024
Executed: gemini --prompt "WebSearch: Node.js latest LTS version 2024 release notes"
```

## Best Practices

- Be specific rather than general
- Include error codes or exact messages
- Mention specific versions or releases
- Add context about your use case
- Request official sources when needed

## Result Format

Results will include:
- **Summary**: Key findings from the search
- **Sources**: Links to original content
- **Relevance**: How well results match your query
- **Date**: When information was published/updated
- **Recommendations**: Next steps or related searches

## Limitations

- Results depend on Gemini API availability
- Some regions may have restricted access
- Rate limits may apply for extensive searches
- Real-time data may have slight delays

## Ready to Search

Please provide your search query and any specific requirements, and I'll execute it using the Gemini CLI for the most accurate results.
