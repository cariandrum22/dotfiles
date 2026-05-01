## Linter and Formatter Configuration Policy - Tamarin

### Required Tools

1. **Tamarin Prover** (REQUIRED - Main Tool)
   - Security protocol verification tool
   - Install: https://tamarin-prover.github.io/manual/book/002_installation.html
   - Run: `tamarin-prover interactive Protocol.spthy`
   - Verify: `tamarin-prover --version`

2. **GraphViz** (REQUIRED - Visualization)
   - For protocol graph visualization
   - Install: `apt-get install graphviz` or `brew install graphviz`

3. **Maude** (REQUIRED - Backend)
   - Term rewriting system used by Tamarin
   - Required for equational theories
   - Install as per Tamarin documentation

### Project Structure

```
tamarin-protocol/
├── protocols/
│   ├── MainProtocol.spthy      # Main protocol specification
│   ├── SubProtocols.spthy      # Modular sub-protocols
│   └── Variants/               # Protocol variants
│       ├── ProtocolV1.spthy
│       └── ProtocolV2.spthy
├── oracles/                    # Oracle files for proof guidance
│   └── protocol.oracle
├── theories/                   # Custom equational theories
│   └── CustomCrypto.spthy
├── models/                     # Threat models
│   └── DYModel.spthy
├── tests/                      # Test cases
│   ├── BasicTests.spthy
│   └── AttackScenarios.spthy
├── Makefile                    # Automation
└── README.md                   # Documentation
```

### Tamarin Style Guidelines

1. **Theory Header**:
   ```tamarin
   /*******************************************************************************
    * Protocol: [Protocol Name]
    * Purpose: [Brief description of the protocol]
    * Author: [Your Name]
    * Date: [Date]
    * Version: [Version]
    *
    * Status: [Draft/Review/Verified]
    ******************************************************************************/
   
   theory ProtocolName
   begin
   
   /* Import built-in theories */
   builtins: diffie-hellman, signing, symmetric-encryption, hashing
   
   /* Import custom theories if needed */
   functions: mac/2, verify_mac/3
   ```

2. **Naming Conventions**:
   ```tamarin
   /* Rules: Snake_Case with descriptive names */
   rule Client_Init:
       [ Fr(~id) ]
     --[ ClientStarted($A, ~id) ]->
       [ ClientState($A, ~id, 'init') ]
   
   /* Facts: CamelCase, prefixed by purpose */
   /* Persistent facts: ! prefix */
   !LongTermKey($A, ~ltk)
   
   /* Linear facts: No prefix */
   ClientState($A, ~id, 'waiting')
   
   /* Variables:
      - ~var: Fresh values
      - $var: Public values
      - var: General variables
   */
   ```

3. **Function Declarations**:
   ```tamarin
   /* Constructors and destructors must be paired */
   functions: 
       /* Encryption */
       aenc/2,        // Asymmetric encryption
       adec/2,        // Asymmetric decryption
       
       /* Digital signatures */
       sign/2,        // Signing
       verify/3,      // Verification: verify(msg, sig, pk) = true
       
       /* MAC functions */
       mac/2,         // Message authentication code
       
       /* Custom functions with equations */
       h/1            // Hash function (one-way)
       
   equations:
       /* Decryption */
       adec(aenc(m, pk(k)), k) = m,
       
       /* Signature verification */
       verify(m, sign(m, k), pk(k)) = true
   ```

4. **Rule Structure**:
   ```tamarin
   rule Rule_Name:
       let
           /* Variable bindings for clarity */
           message = <'Type', $A, $B, ~nonce>
           signature = sign(message, ltkA)
           encrypted = aenc(<message, signature>, pkB)
       in
       [ /* Premises (consumed facts) */
         ClientState($A, ~id, 'init'),
         !LongTermKey($A, ltkA),
         !PublicKey($B, pkB)
       ]
     --[ /* Action facts (for lemmas) */
         Send($A, $B, encrypted),
         Honest($A),
         Honest($B),
         SecretGenerated(~nonce)
       ]->
       [ /* Conclusions (produced facts) */
         ClientState($A, ~id, 'waiting_response'),
         Out(encrypted),
         Secret($A, $B, ~nonce)
       ]
   ```

### Security Properties

1. **Authentication Lemmas**:
   ```tamarin
   /* Aliveness */
   lemma aliveness:
       "All a b t #i. 
        Commit(a, b, t)@i ==> 
        (Ex #j. Running(b, a, t)@j) | (Ex #j. Corrupt(b)@j)"
   
   /* Weak agreement */
   lemma weak_agreement:
       "All a b #i. 
        Commit(a, b)@i ==> 
        (Ex #j. Running(b, a)@j & j < i) | (Ex #j. Corrupt(b)@j)"
   
   /* Non-injective agreement */
   lemma non_injective_agreement:
       "All a b t #i. 
        Commit(a, b, <'DATA', t>)@i ==> 
        (Ex #j. Running(b, a, <'DATA', t>)@j & j < i) | 
        (Ex #j. Corrupt(b)@j)"
   
   /* Injective agreement */
   lemma injective_agreement:
       "All a b t #i. 
        Commit(a, b, <'DATA', t>)@i ==> 
        (Ex #j. Running(b, a, <'DATA', t>)@j & j < i & 
         not(Ex a2 b2 #i2. Commit(a2, b2, <'DATA', t>)@i2 & 
         not(#i = #i2))) |
        (Ex #j. Corrupt(b)@j)"
   ```

2. **Secrecy Lemmas**:
   ```tamarin
   /* Basic secrecy */
   lemma secret_value:
       "All x #i. 
        Secret(x)@i ==> 
        not(Ex #j. K(x)@j) | 
        (Ex #j. Corrupt(x)@j & j < i)"
   
   /* Forward secrecy */
   lemma forward_secrecy:
       "All x #i #j. 
        Secret(x)@i & K(x)@j ==> 
        (Ex #k. Corrupt(x)@k & i < k & k < j)"
   
   /* Perfect forward secrecy */
   lemma perfect_forward_secrecy:
       "All sess key #i #j.
        SessionKey(sess, key)@i & K(key)@j ==>
        (Ex actor #k. 
         SessionInvolves(sess, actor)@i & 
         Corrupt(actor)@k & i < k)"
   ```

3. **Privacy Properties**:
   ```tamarin
   /* Observational equivalence */
   lemma anonymity:
       "All x y #i #j.
        Choice(x, y)@i & K(x)@j ==> F"
   ```

### Best Practices

1. **Modular Protocol Design**:
   ```tamarin
   /* Separate initialization from protocol logic */
   rule Initialize_Keys:
       [ Fr(~ltk) ]
     --[ KeyGen($A, ~ltk) ]->
       [ !LongTermKey($A, ~ltk),
         !PublicKey($A, pk(~ltk)),
         Out(pk(~ltk)) ]
   
   /* Modular sub-protocols */
   restriction SubProtocol_Ordering:
       "All x #i #j. 
        SubProtocol_End(x)@i & SubProtocol_Start(x)@j ==> j < i"
   ```

2. **Attack Modeling**:
   ```tamarin
   /* Explicit corruption */
   rule Corrupt_LongTermKey:
       [ !LongTermKey($A, ~ltk) ]
     --[ Corrupt($A) ]->
       [ Out(~ltk) ]
   
   /* State reveal for forward secrecy */
   rule Reveal_State:
       [ ClientState($A, ~id, state) ]
     --[ RevealState($A, ~id) ]->
       [ Out(state) ]
   ```

3. **Restrictions for Well-formedness**:
   ```tamarin
   /* Ensure unique initialization */
   restriction unique_init:
       "All A #i #j. Init(A)@i & Init(A)@j ==> #i = #j"
   
   /* Temporal ordering */
   restriction response_after_challenge:
       "All A B x #i #j. 
        Response(A, B, x)@i & Challenge(A, B, x)@j ==> j < i"
   
   /* Type safety */
   restriction keys_distinct:
       "All k #i #j. 
        IsSigningKey(k)@i & IsEncryptionKey(k)@j ==> F"
   ```

### Proof Strategies

1. **Automatic Proof**:
   ```bash
   # Basic automatic proof
   tamarin-prover --prove Protocol.spthy
   
   # With heuristics
   tamarin-prover --prove --heuristic=O --oraclename=oracle.py Protocol.spthy
   ```

2. **Interactive Proof**:
   ```tamarin
   /* Helper lemmas for complex proofs */
   lemma helper_unique_nonce [reuse, use_induction]:
       "All n #i #j. Fresh(n)@i & Fresh(n)@j ==> #i = #j"
   
   /* Proof annotations */
   lemma complex_property [use_induction, heuristic=C]:
       "..."
   ```

3. **Oracle Guidance**:
   ```python
   #!/usr/bin/env python
   # oracle.py
   
   def oracle(lemma, proof_state):
       goal = proof_state['goal']
       
       # Prioritize certain goals
       if 'Fresh' in goal:
           return 1  # High priority
       elif 'K(' in goal:
           return 0  # Low priority
       
       return None  # Default priority
   ```

### Common Patterns

1. **Handshake Protocol**:
   ```tamarin
   /* Three-way handshake pattern */
   rule Client_Hello:
       [ Fr(~nc) ]
     --[ ClientHello($C, $S, ~nc) ]->
       [ Out(<'hello', $C, $S, ~nc>),
         ClientWait($C, $S, ~nc) ]
   
   rule Server_Response:
       [ In(<'hello', $C, $S, nc>),
         Fr(~ns),
         !LongTermKey($S, ~ltkS) ]
     --[ ServerResponse($S, $C, nc, ~ns) ]->
       [ Out(<'response', $S, $C, nc, ~ns, sign(<nc, ~ns>, ~ltkS)>),
         ServerWait($S, $C, nc, ~ns) ]
   
   rule Client_Confirm:
       [ ClientWait($C, $S, ~nc),
         In(<'response', $S, $C, ~nc, ns, sig>),
         !PublicKey($S, pkS) ]
     --[ Eq(verify(<~nc, ns>, sig, pkS), true),
         ClientConfirm($C, $S, ~nc, ns) ]->
       [ ClientDone($C, $S, ~nc, ns) ]
   ```

2. **Key Exchange Pattern**:
   ```tamarin
   /* Diffie-Hellman key exchange */
   rule DH_Init:
       [ Fr(~x) ]
     --[ DHInit($A, ~x) ]->
       [ Out('g'^~x),
         DHState($A, ~x) ]
   
   rule DH_Complete:
       [ DHState($A, ~x),
         In('g'^y) ]
     --[ DHComplete($A, 'g'^(~x*y)) ]->
       [ !SharedKey($A, 'g'^(~x*y)) ]
   ```

### Testing and Validation

1. **Sanity Checks**:
   ```tamarin
   /* Ensure protocol can complete */
   lemma sanity_completion:
       exists-trace
       "Ex A B #i. ProtocolComplete(A, B)@i"
   
   /* Check if attack is possible */
   lemma sanity_attack_possible:
       exists-trace
       "Ex x #i. Secret(x)@i & Ex #j. K(x)@j"
   ```

2. **Makefile for Automation**:
   ```makefile
   TAMARIN = tamarin-prover
   
   all: verify
   
   verify:
   	$(TAMARIN) --prove MainProtocol.spthy
   
   interactive:
   	$(TAMARIN) interactive MainProtocol.spthy
   
   clean:
   	rm -f *.cache
   
   test:
   	$(TAMARIN) --prove tests/BasicTests.spthy
   ```

### Editor Support

**VSCode**:
- Install "Tamarin Language Support" extension
- Provides syntax highlighting and snippets

**Vim**:
- Tamarin syntax file available in Tamarin repository
- Copy to `~/.vim/syntax/spthy.vim`

### Common Pitfalls

1. **Missing freshness assumptions** - Always use Fr() for nonces
2. **Incorrect equation ordering** - Equations must be confluent
3. **Unbounded loops** - Use restrictions to bound protocol runs
4. **Missing corruption rules** - Model all compromise scenarios
5. **Overly complex rules** - Break down into smaller steps
6. **Implicit assumptions** - Make all assumptions explicit as restrictions