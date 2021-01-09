using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace CsPoker
{
    enum Suits { Diamonds, Hearts, Spades, Clubs }
    enum Players { P1, P2 }
    enum Ranks { Distinct, Pair, TwoPairs, ThreeOfAKind, FullHouse, FourOfAKind }
    record Card(Suits Suit, int Val);
    record Hand(Card[] Cards);
    record HandRank(Ranks Rank, int Value, int[] Remaining);
    class Program {
        static void Main(string[] _) {
            int winners1 = 0, winners2 = 0;

            while(Console.ReadLine() is string line && !string.IsNullOrWhiteSpace(line)) {
                switch(CalcWinner(GetHands(line))) {
                    case Players.P1: ++winners1; break;
                    case Players.P2: ++winners2 ; break;
                }
            }

            Console.WriteLine($"Player 1: {winners1}");
            Console.WriteLine($"Player 2: {winners2}");
            Console.ReadLine();
        }

        static (Hand, Hand) GetHands(string line) {
            Hand ToHand(string[] cards) {
                Card ToCard(string card) {
                    Suits ToSuit(char c) => c switch {
                        'D' => Suits.Diamonds, 'H' => Suits.Hearts, 'S' => Suits.Spades, 'C' => Suits.Clubs,
                        _ => throw new Exception($"Wrong suit {c}"),
                    };
                    int ToValue(char c) => c switch {
                        >= '2' and <= '9' => c - '0',
                        'T' => 10, 'J' => 11, 'Q' => 12, 'K' => 13, 'A' => 14,
                        _ => throw new Exception($"Wrong value {c}"),
                    };
                    return new Card(ToSuit(card[1]), ToValue(card[0]));
                }
                return new Hand(cards.Select(ToCard).ToArray());
            }

            return line.Split(" ") switch {
                string[] { Length: 10 } cards when cards.All(card => card.Length == 2) => (ToHand(cards[..5]), ToHand(cards[5..])),
                _ => throw new Exception("Wrong Input"),
            };
        }

        static Players CalcWinner((Hand h1, Hand h2) hands) {
            (int score, int[] remaining) Score(Hand h) {
                HandRank RankHand(Hand h) {
                    return h.Cards.GroupBy(card => card.Val).OrderByDescending(group => group.Count()).Select(g => g.ToArray()).ToArray() switch {
                        Card[][] { Length:2 } groups when groups[0].Length == 4 => new HandRank(Ranks.FourOfAKind, groups[0][0].Val, groups[1].Select(card => card.Val).ToArray()),
                        Card[][] { Length:2 } groups when groups[0].Length == 3 => new HandRank(Ranks.FourOfAKind, groups[0][0].Val, new[] { groups[1][0].Val }),
                        Card[][] { Length:3 } groups when groups[0].Length == 3 => new HandRank(Ranks.ThreeOfAKind, groups[0][0].Val, new[] { groups[1][0].Val, groups[2][0].Val }),
                        Card[][] { Length:3 } groups when groups[0].Length == 2 => new HandRank(Ranks.TwoPairs, Math.Max(groups[0][0].Val, groups[1][0].Val), new[] { Math.Min(groups[0][0].Val, groups[1][0].Val), groups[2][0].Val }),
                        Card[][] { Length:4 } groups when groups[0].Length == 2 => new HandRank(Ranks.Pair, groups[0][0].Val, new[] { groups[1][0].Val, groups[2][0].Val, groups[3][0].Val }),
                        _ => ((Func<HandRank>)(() => {
                            var ordered = h.Cards.Select(card => card.Val).OrderByDescending(i => i).ToArray();
                            return new HandRank(Ranks.Distinct, h.Cards.All(card => card.Suit == h.Cards[0].Suit) ? 1 : 0, ordered);
                        }))(),

                    };
                }

                return RankHand(h) switch {
                    HandRank { Rank: Ranks.Distinct, Value: var sameSuit, Remaining: var ordered } => ((Func<(int, int[])>)(() => {
                        if (ordered[0] - ordered[4] == 4) { //Straight, Straight Flush or Royal Flush
                            if (sameSuit == 1) { //Straight Flush or Royal Flush
                                if (ordered[0] == 14) return (1000, new int[] { }); //Royal Flush
                                else return (900 + ordered[0], new int[] { }); //Straight Flush
                            }
                            else return (500 + ordered[0], new int[] { }); //Straight
                        }
                        else {//Flush or High Card
                            if (sameSuit == 1) return (600 + ordered[0], ordered[1..]); //Flush
                            else return (100 + ordered[0], ordered[1..]); //High Card
                        }
                    }))(),

                    HandRank { Rank: Ranks.Pair, Value: var pairValue, Remaining: var remaining3Ordered } => (200 + pairValue, remaining3Ordered),
                    HandRank { Rank: Ranks.TwoPairs, Value: var highestPairValue, Remaining: var remaining1 } => (300 + highestPairValue, remaining1),
                    HandRank { Rank: Ranks.ThreeOfAKind, Value: var trisValue, Remaining: var remaining2 } => (400 + trisValue, remaining2),
                    HandRank { Rank: Ranks.FullHouse, Value: var trisValue, Remaining: var pairValue } => (700 + trisValue, pairValue),
                    HandRank { Rank: Ranks.FourOfAKind, Value: var pokerValue, Remaining: var remaining1 } => (800 + pokerValue, remaining1),
                    HandRank hr => throw new Exception($"Internal Error (HandRank = {hr})"),
                    _ => throw new Exception("Internal Error (HandRank = null)"),
                };
            }

            return (Score(hands.h1), Score(hands.h2)) switch {
                ((var score1, _), (var score2, _)) when score1 > score2 => Players.P1,
                ((var score1, _), (var score2, _)) when score1 < score2 => Players.P2,
                ((_, var rem1), (_, var rem2)) => ((Func<int[], int[], Players>)((rem1, rem2) => {
                    Array.Sort(rem1, descendingComparer);
                    Array.Sort(rem2, descendingComparer);
                    if (rem1.GreaterThan(rem2)) { return Players.P1; }
                    else
                    if (rem2.GreaterThan(rem1)) { return Players.P2; }
                    throw new Exception("Tie");
                }))(rem1, rem2),
            };
        }
        static Comparison<int> descendingComparer = new Comparison<int>((i1, i2) => i2.CompareTo(i1));
    }

    static class Extensions
    {
        public static bool GreaterThan(this int[] a1, int[] a2) {
            if (a1.Length != a2.Length) throw new Exception("Remaining arrays should have equal lengths");
            for (int i = 0; i < a1.Length; i++) if (a1[i] != a2[i]) return a1[i] > a2[i];
            return false;
        }
    }
}
