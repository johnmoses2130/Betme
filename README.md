# 🏆 Betme - Peer Challenge Betting Contract

A Clarity smart contract for creating friendly fitness and study challenges with peer-to-peer betting on the Stacks blockchain.

## 🚀 Features

- 💪 Create fitness/study challenges with STX stakes
- 🤝 Accept challenges from other users
- 🗳️ Community voting system to determine winners
- 💰 Automatic prize distribution
- 📊 User statistics tracking
- ⚖️ Tie handling with refunds

## 🎯 How It Works

1. **Create Challenge**: Set a title, description, stake amount, and deadline
2. **Accept Challenge**: Another user can accept and match the stake
3. **Complete Challenge**: Participants work towards their goals
4. **Community Voting**: Other users vote on who completed the challenge
5. **Claim Winnings**: Winner takes the total pot (2x stake amount)

## 📋 Contract Functions

### Public Functions

- `create-challenge` - Create a new challenge
- `accept-challenge` - Accept an existing challenge
- `submit-completion` - Mark challenge as ready for voting
- `vote-on-challenge` - Vote for the winner
- `finalize-challenge` - Determine winner based on votes
- `claim-winnings` - Winner claims the prize
- `claim-tie-refund` - Get refund in case of tie

### Read-Only Functions

- `get-challenge` - Get challenge details
- `get-user-stats` - Get user statistics
- `get-challenge-count` - Get total number of challenges
- `get-user-vote` - Check how a user voted

## 🛠️ Usage Examples

### Creating a Challenge
```clarity
(contract-call? .Betme create-challenge 
  "30-Day Push-up Challenge" 
  "Do 100 push-ups daily for 30 days" 
  u1000000 
  u4320)
```

### Accepting a Challenge
```clarity
(contract-call? .Betme accept-challenge u1)
```

### Voting on a Challenge
```clarity
(contract-call? .Betme vote-on-challenge u1 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

## 🎮 Challenge Lifecycle

1. **Open** 📝 - Challenge created, waiting for opponent
2. **Active** ⚡ - Both participants committed, challenge in progress
3. **Voting** 🗳️ - Deadline reached, community voting phase
4. **Completed** ✅ - Winner determined, ready for prize claim
5. **Tie** 🤝 - Equal votes, participants can claim refunds

## 💡 Challenge Ideas

- 🏃‍♂️ **Fitness**: Daily runs, gym sessions, step counts
- 📚 **Study**: Reading goals, coding challenges, language learning
- 🎨 **Creative**: Daily drawings, writing streaks, music practice
- 🧘‍♀️ **Wellness**: Meditation, sleep tracking, healthy eating

## ⚠️ Important Notes

- Voting period lasts 144 blocks (~24 hours) after challenge deadline
- Participants cannot vote on their own challenges
- Stakes are held in the contract until completion
- Community decides winners through democratic voting

## 🔧 Development

Built with Clarinet for the Stacks blockchain. Deploy using:

```bash
clarinet deploy
```

## 🤝 Contributing

Feel free to submit issues and enhancement requests!

---

*Challenge yourself, challenge others, and let the community decide! 🎯*