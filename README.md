# Carbonet

# 🌳 Carbonet: Carbon Offset Marketplace

A decentralized marketplace for trading carbon credits on the Stacks blockchain.

## 🎯 Features

- Register carbon offset projects
- Verify projects through authorized verifiers
- Purchase carbon credits
- Transfer credits between users
- Track credit balances
- NFT representation of projects

## 🚀 Getting Started

### Prerequisites

- Clarinet
- Stacks wallet

### Contract Functions

#### For Project Owners
- `register-project`: Register a new carbon offset project
- `transfer-credits`: Transfer credits to another user

#### For Verifiers
- `verify-project`: Verify a registered project

#### For Buyers
- `purchase-credits`: Purchase credits from verified projects

#### For Contract Owner
- `add-verifier`: Add authorized verifiers
- `remove-verifier`: Remove verifier access

### Read-Only Functions
- `get-project`: Get project details
- `get-credit-balance`: Check credit balance
- `get-verifier-status`: Check verifier status

## 💡 Usage Example

1. Register a new project:
```clarity
(contract-call? .carbonet register-project "Forest Conservation" "Amazon" u1000)
```

2. Add a verifier:
```clarity
(contract-call? .carbonet add-verifier 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
```

3. Purchase credits:
```clarity
(contract-call? .carbonet purchase-credits u1 u100)
```

## 🔒 Security

- Owner-only administrative functions
- Verification requirements for credit purchases
- Balance checks for transfers
```


