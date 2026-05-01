## Linter and Formatter Configuration Policy - TLA+

### Required Tools

1. **TLA+ Toolbox** (REQUIRED - IDE)
   - Official IDE with integrated model checker
   - Download: https://lamport.azurewebsites.net/tla/toolbox.html
   - Includes TLC model checker and PlusCal translator

2. **tla-format** (RECOMMENDED - Formatter)
   - Community formatter for TLA+ specifications
   - Install: `cargo install tla-format`
   - Run: `tla-format *.tla`
   - Note: Still in development, may have limitations

3. **TLC** (REQUIRED - Model Checker)
   - Validates specifications against properties
   - Integrated in TLA+ Toolbox
   - CLI: `java -jar tla2tools.jar -config Model.cfg Spec.tla`

4. **TLAPS** (OPTIONAL - Proof System)
   - For mathematical proofs of correctness
   - Install separately for advanced verification

### Project Structure

```
tla-project/
├── Specification.tla       # Main specification
├── Specification.cfg       # TLC model configuration
├── SpecificationMC.tla    # Model checking instance (if needed)
├── Invariants.tla         # Reusable invariants module
├── Properties.tla         # Temporal properties module
├── TypeInvariants.tla     # Type checking predicates
├── Tests/                 # Test scenarios
│   ├── TestScenario1.cfg
│   └── TestScenario2.cfg
└── README.md              # Specification documentation
```

### TLA+ Style Guidelines

1. **Module Structure**:
   ```tla
   ----------------------------- MODULE SpecName -----------------------------
   (**************************************************************************)
   (* Purpose: Brief description of what this specification models           *)
   (* Author: Your Name                                                      *)
   (* Date: 2024-01-01                                                      *)
   (**************************************************************************)
   
   EXTENDS Integers, Sequences, FiniteSets, TLC
   
   ---- Module Parameters ----
   CONSTANTS 
       Nodes,          \* Set of nodes in the system
       Values,         \* Set of possible values
       MaxRetries      \* Maximum retry attempts
   
   ASSUME
       /\ Cardinality(Nodes) > 0
       /\ Cardinality(Values) > 0
       /\ MaxRetries \in Nat
       /\ MaxRetries > 0
   
   ---- State Variables ----
   VARIABLES
       state,          \* Current state of each node
       messages,       \* Messages in transit
       history         \* Event history for debugging
   
   vars == <<state, messages, history>>
   
   ---- Type Invariant ----
   TypeOK ==
       /\ state \in [Nodes -> States]
       /\ messages \subseteq Messages
       /\ history \in Seq(Events)
   ```

2. **Naming Conventions**:
   ```tla
   \* Constants: UpperCamelCase
   CONSTANT MaxValue, TimeoutDuration
   
   \* Variables: lowerCamelCase
   VARIABLE currentState, messageQueue
   
   \* Operators: UpperCamelCase for main logic
   Init == ...
   Next == ...
   
   \* Helper operators: lowerCamelCase
   isValid(msg) == ...
   canProgress(node) == ...
   
   \* Sets and predicates: descriptive names
   ValidMessages == ...
   SafetyInvariant == ...
   ```

3. **Documentation Standards**:
   ```tla
   (***************************************************************************
    This operator models the state transition when a node receives a message.
    
    Parameters:
      - n: The node receiving the message
      - m: The message being processed
      
    Preconditions:
      - The message must be addressed to node n
      - The node must be in a state that can handle this message type
      
    Effects:
      - Updates the node's state
      - May generate response messages
      - Records the event in history
    ***************************************************************************)
   ProcessMessage(n, m) ==
       /\ m.dest = n
       /\ state[n].status = "ready"
       /\ state' = [state EXCEPT ![n] = ...]
       /\ messages' = messages \ {m} \union ResponseMessages(n, m)
       /\ history' = Append(history, [type |-> "process", node |-> n, msg |-> m])
   ```

### Specification Best Practices

1. **Separate Concerns**:
   ```tla
   ---- Safety Properties ----
   NoDoubleDelivery ==
       \A m \in DeliveredMessages :
           Cardinality({e \in history : e.type = "deliver" /\ e.msg = m}) <= 1
   
   Consistency ==
       \A n1, n2 \in Nodes :
           Agreed(n1) /\ Agreed(n2) => state[n1].value = state[n2].value
   
   ---- Liveness Properties ----
   EventuallyDelivered ==
       \A m \in SentMessages :
           <>[](m \in DeliveredMessages)
   
   Progress ==
       \A n \in Nodes :
           [](state[n].status = "waiting" => <>(state[n].status = "done"))
   ```

2. **Type Safety**:
   ```tla
   ---- Type Definitions ----
   States == [
       status: {"init", "ready", "processing", "done", "failed"},
       value: Values \union {Null},
       timestamp: Nat
   ]
   
   Messages == [
       type: {"request", "response", "heartbeat"},
       src: Nodes,
       dest: Nodes,
       payload: Values,
       id: Nat
   ]
   
   ---- Type Checking ----
   MessageTypeOK(m) ==
       /\ m.type \in {"request", "response", "heartbeat"}
       /\ m.src \in Nodes
       /\ m.dest \in Nodes
       /\ m.src # m.dest
       /\ m.payload \in Values
       /\ m.id \in 1..MaxMessageId
   ```

3. **State Space Management**:
   ```tla
   ---- Symmetry and State Reduction ----
   Symmetry == Permutations(Nodes)
   
   ---- View for State Deduplication ----
   View == <<
       [n \in Nodes |-> state[n].status],
       Cardinality(messages),
       Len(history)
   >>
   
   ---- State Constraints ----
   StateConstraint ==
       /\ Len(history) <= MaxHistoryLength
       /\ Cardinality(messages) <= MaxMessages
       /\ \A n \in Nodes : state[n].timestamp <= MaxTimestamp
   ```

### Model Configuration (.cfg files)

```cfg
SPECIFICATION Spec
CONSTANTS
    Nodes = {n1, n2, n3}
    Values = {v1, v2}
    MaxRetries = 3
    Null = Null
    
INVARIANTS
    TypeOK
    NoDoubleDelivery
    Consistency
    
PROPERTIES
    EventuallyDelivered
    Progress
    
SYMMETRY
    Symmetry
    
CONSTRAINT
    StateConstraint
    
VIEW
    View
    
\* Check specific scenarios
INIT
    InitWithFailure
    
\* Limit state space for initial testing
CHECK_DEADLOCK
    FALSE
```

### PlusCal Usage (When Appropriate)

```tla
--algorithm DistributedConsensus {
    variables
        state = [n \in Nodes |-> "init"],
        messages = {},
        decided = [n \in Nodes |-> FALSE];
        
    define {
        AllDecided == \A n \in Nodes : decided[n]
        Agreement == \A n1, n2 \in Nodes : 
            decided[n1] /\ decided[n2] => state[n1] = state[n2]
    }
    
    macro send(msg) {
        messages := messages \union {msg}
    }
    
    macro receive(msg) {
        messages := messages \ {msg}
    }
    
    process (node \in Nodes)
    variable proposal = CHOOSE v \in Values : TRUE;
    {
        Propose:
            send([type |-> "propose", src |-> self, value |-> proposal]);
            
        Collect:
            while (~decided[self]) {
                with (msg \in {m \in messages : m.dest = self}) {
                    receive(msg);
                    \* Process message logic
                }
            }
    }
}
```

### Testing Strategies

1. **Small Model Testing**:
   ```cfg
   \* Start with minimal configuration
   CONSTANTS
       Nodes = {n1, n2}
       Values = {v1}
       MaxRetries = 1
   ```

2. **Incremental Complexity**:
   ```cfg
   \* Gradually increase complexity
   CONSTANTS
       Nodes = {n1, n2, n3, n4}
       Values = {v1, v2, v3}
       MaxRetries = 5
   ```

3. **Error Injection**:
   ```tla
   \* Model failures explicitly
   NodeFailure(n) ==
       /\ state[n].status # "failed"
       /\ state' = [state EXCEPT ![n].status = "failed"]
       /\ messages' = messages \ {m \in messages : m.src = n}
       /\ UNCHANGED history
   ```

### Common Patterns

1. **Request-Response Pattern**:
   ```tla
   SendRequest(client, server, value) ==
       /\ messages' = messages \union {[
              type |-> "request",
              src |-> client,
              dest |-> server,
              payload |-> value,
              id |-> NextId()
          ]}
       /\ UNCHANGED <<state, history>>
       
   HandleResponse(client, response) ==
       /\ response.dest = client
       /\ response.type = "response"
       /\ state' = [state EXCEPT ![client] = ProcessResponse(response)]
       /\ messages' = messages \ {response}
       /\ RecordEvent("response_received", client, response)
   ```

2. **Leader Election Pattern**:
   ```tla
   BecomeCandidate(n) ==
       /\ state[n].role = "follower"
       /\ state' = [state EXCEPT 
              ![n].role = "candidate",
              ![n].term = state[n].term + 1]
       /\ SendVoteRequests(n)
       
   ElectLeader(n) ==
       /\ state[n].role = "candidate"
       /\ HasMajority(n)
       /\ state' = [state EXCEPT ![n].role = "leader"]
       /\ AnnounceLeadership(n)
   ```

### Verification Checklist

- [ ] Type invariants defined and checked
- [ ] Safety properties explicitly stated
- [ ] Liveness properties under fairness assumptions
- [ ] State constraints to bound model checking
- [ ] Symmetry reduction where applicable
- [ ] Documentation for all major operators
- [ ] Test configurations for different scenarios
- [ ] Error states and recovery modeled
- [ ] Fairness constraints specified

### Editor Integration

**VSCode**:
- Install "TLA+" extension by alygin
- Provides syntax highlighting and basic checking
- Integrates with TLA+ Toolbox

**Configuration**:
```json
{
  "tlaplus.tlc.modelChecker.options": ["-deadlock", "-coverage", "1"],
  "tlaplus.pluscal.options": ["-nocfg"],
  "[tlaplus]": {
    "editor.tabSize": 4,
    "editor.insertSpaces": true
  }
}
```

### Learning Path

1. Start with simple specifications (mutual exclusion, producer-consumer)
2. Use PlusCal for algorithm-style thinking initially
3. Transition to pure TLA+ for more complex temporal properties
4. Master refinement mappings for implementation verification
5. Learn TLAPS for critical correctness proofs

### Common Pitfalls to Avoid

- Over-specifying implementation details
- Unbounded state spaces without constraints
- Missing fairness assumptions for liveness
- Incorrect initial state specifications
- Forgetting to check type invariants
- Not using symmetry reduction for identical processes