## Linter and Formatter Configuration Policy - JavaScript/TypeScript

### Required Tools

1. **Biome** (PREFERRED - All-in-One Fast Solution)
   - Ultra-fast Rust-based linter/formatter (replaces ESLint + Prettier)
   - Run: `biome check .` or `biome lint .` and `biome format .`
   - Auto-fix: `biome check --apply .`
   - Install: `npm install --save-dev @biomejs/biome`

2. **ESLint + Prettier** (ALTERNATIVE - Traditional Stack)
   - Use only if Biome doesn't support required plugins
   - ESLint: `npm install --save-dev eslint @typescript-eslint/parser @typescript-eslint/eslint-plugin`
   - Prettier: `npm install --save-dev prettier eslint-config-prettier`

3. **TypeScript** (REQUIRED for TS projects)
   - Type checking: `tsc --noEmit`
   - Install: `npm install --save-dev typescript`

### Configuration - Biome (Preferred)

Create `biome.json`:
```json
{
  "$schema": "https://biomejs.dev/schemas/1.4.1/schema.json",
  "organizeImports": {
    "enabled": true
  },
  "linter": {
    "enabled": true,
    "rules": {
      "recommended": true,
      "complexity": {
        "noForEach": "error",
        "noUselessConstructor": "error"
      },
      "style": {
        "noVar": "error",
        "useConst": "error",
        "useTemplate": "error"
      }
    }
  },
  "formatter": {
    "enabled": true,
    "formatWithErrors": false,
    "indentStyle": "space",
    "indentWidth": 2,
    "lineWidth": 100
  },
  "javascript": {
    "formatter": {
      "quoteStyle": "single",
      "trailingComma": "es5",
      "semicolons": "always"
    }
  }
}
```

Add to `package.json`:
```json
{
  "scripts": {
    "lint": "biome check .",
    "lint:fix": "biome check --apply .",
    "format": "biome format --write .",
    "typecheck": "tsc --noEmit"
  }
}
```

### Configuration - ESLint + Prettier (Alternative)

Only use if Biome lacks required features. Create `.eslintrc.js`:
```js
module.exports = {
  parser: '@typescript-eslint/parser',
  extends: [
    'eslint:recommended',
    'plugin:@typescript-eslint/recommended',
    'plugin:@typescript-eslint/recommended-requiring-type-checking',
    'prettier'
  ],
  parserOptions: {
    project: './tsconfig.json',
    ecmaVersion: 'latest',
    sourceType: 'module'
  },
  rules: {
    '@typescript-eslint/explicit-function-return-type': 'error',
    '@typescript-eslint/no-explicit-any': 'error',
    '@typescript-eslint/no-unused-vars': ['error', { argsIgnorePattern: '^_' }]
  }
};
```

Create `.prettierrc`:
```json
{
  "semi": true,
  "trailingComma": "es5",
  "singleQuote": true,
  "printWidth": 100,
  "tabWidth": 2
}
```

### TypeScript Configuration

Create `tsconfig.json`:
```json
{
  "compilerOptions": {
    "target": "ES2022",
    "module": "commonjs",
    "lib": ["ES2022"],
    "strict": true,
    "esModuleInterop": true,
    "skipLibCheck": true,
    "forceConsistentCasingInFileNames": true,
    "resolveJsonModule": true,
    "noUnusedLocals": true,
    "noUnusedParameters": true,
    "noImplicitReturns": true,
    "noFallthroughCasesInSwitch": true,
    "noUncheckedIndexedAccess": true,
    "exactOptionalPropertyTypes": true
  },
  "include": ["src/**/*"],
  "exclude": ["node_modules", "dist", "build"]
}
```

### Pre-commit Hook

Using husky + lint-staged:
```json
{
  "lint-staged": {
    "*.{js,jsx,ts,tsx}": [
      "biome check --apply",
      "git add"
    ]
  }
}
```

### Editor Integration

**VSCode** - For Biome:
```json
{
  "editor.defaultFormatter": "biomejs.biome",
  "editor.formatOnSave": true,
  "[javascript]": {
    "editor.defaultFormatter": "biomejs.biome"
  },
  "[typescript]": {
    "editor.defaultFormatter": "biomejs.biome"
  }
}
```

### CI/CD Integration

GitHub Actions:
```yaml
- name: Setup Node
  uses: actions/setup-node@v4
  with:
    node-version: '20'
    cache: 'npm'
    
- name: Install dependencies
  run: npm ci
  
- name: Lint and Format Check
  run: |
    npm run lint
    npm run typecheck
```

### Framework-Specific Additions

**React**: 
- Add React-specific Biome rules or ESLint plugin

**Vue**: 
- May require ESLint due to template parsing needs

**Next.js**: 
- Use built-in `next lint` alongside Biome

### Migration Path

When migrating existing projects:
1. Run Biome with `--apply-unsafe` to auto-fix most issues
2. Gradually enable stricter rules
3. Remove ESLint/Prettier configs after successful migration

### Functional Programming Patterns

JavaScript/TypeScript has **strong functional support**. Enforce these patterns:

1. **Immutability**:
   ```typescript
   // Good: Use const by default
   const user = { name: 'Alice', age: 30 };
   const updated = { ...user, age: 31 }; // Spread operator
   
   // Good: Array methods that return new arrays
   const doubled = numbers.map(n => n * 2);
   const filtered = items.filter(item => item.active);
   
   // Avoid: Mutations
   user.age = 31; // Mutating object
   numbers.push(4); // Mutating array
   ```

2. **Pure Functions**:
   ```typescript
   // Good: Pure function
   const calculateTotal = (items: Item[]): number =>
     items.reduce((sum, item) => sum + item.price, 0);
   
   // Avoid: Side effects in computation
   let total = 0;
   const calculateTotal = (items: Item[]): number => {
     items.forEach(item => { total += item.price }); // Side effect
     return total;
   };
   ```

3. **Function Composition**:
   ```typescript
   // Good: Compose with pipe
   const pipe = <T>(...fns: Array<(arg: T) => T>) =>
     (value: T): T => fns.reduce((acc, fn) => fn(acc), value);
   
   const processData = pipe(
     normalize,
     validate,
     transform
   );
   
   // Good: Method chaining
   const result = data
     .filter(item => item.active)
     .map(item => item.value)
     .reduce((sum, val) => sum + val, 0);
   ```

4. **Higher-Order Functions**:
   ```typescript
   // Good: Functions returning functions
   const multiply = (factor: number) => (value: number) => value * factor;
   const double = multiply(2);
   
   // Good: Functions accepting functions
   const retry = <T>(fn: () => Promise<T>, times: number): Promise<T> =>
     fn().catch(err => times > 0 ? retry(fn, times - 1) : Promise.reject(err));
   ```

5. **Type-Safe Functional Patterns**:
   ```typescript
   // Good: Option/Result types
   type Option<T> = { tag: 'Some'; value: T } | { tag: 'None' };
   type Result<T, E> = { tag: 'Ok'; value: T } | { tag: 'Err'; error: E };
   
   // Good: Exhaustive pattern matching
   const handleOption = <T>(option: Option<T>): string => {
     switch (option.tag) {
       case 'Some': return `Value: ${option.value}`;
       case 'None': return 'No value';
     }
   };
   ```

6. **Biome Rules for Functional Style**:
   ```json
   {
     "linter": {
       "rules": {
         "complexity": {
           "noForEach": "error",  // Prefer map/filter/reduce
           "useLiteralKeys": "error"
         },
         "style": {
           "useConst": "error",  // Immutability by default
           "noVar": "error",
           "noParameterAssign": "error"  // Don't mutate parameters
         }
       }
     }
   }
   ```

7. **Avoid Imperative Patterns**:
   - Replace `for` loops with `map`, `filter`, `reduce`
   - Replace `forEach` with appropriate functional methods
   - Replace class-based OOP with functional composition
   - Use readonly arrays and objects in TypeScript