# Gemini Web Search

Use Gemini CLI for web search only when the user explicitly asks to use Gemini
or Gemini CLI. Do not claim it is more accurate than other available search
tools.

## Prerequisites

Before searching:

1. Verify `gemini` is available.
2. Verify Gemini CLI can start with the current configuration.
3. If credentials or local Gemini configuration are broken, report the concrete
   startup error and stop.

## Search Process

When the user provides a query:

1. Format the query for search.
2. Execute Gemini CLI in non-interactive mode.
3. Parse the result for relevant claims and source links.
4. Present findings with source attribution when Gemini returns sources.

## Query Format

The command uses the following format:

```bash
gemini --prompt "WebSearch: <query>"
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

## Usage

If the user already provided a query, run the search. Ask a follow-up question
only when the query is missing or the requested constraints are ambiguous enough
to change the result materially.

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
