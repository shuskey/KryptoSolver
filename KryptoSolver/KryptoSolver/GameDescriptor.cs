using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FSharpKrypto;

namespace KryptoSolver
{
    public class GameDescriptor
    {

        public GameType GameType = GameType.Undefined;
        public string Description = null;
        public string DealButtonText = null;
        public string SolveButtonText = null;
        public int NumberOfOperands = 0;
        public Boolean useExponents = false;
        public Boolean useModulus = false;
        public Boolean findClosest = false;
        public RandomizerType RandomizerType = RandomizerType.Undefined;
        private Random random = new Random();
        private CardDeck deck = new CardDeck();
        
        public GameDescriptor(GameType gameType)
        {
            switch (gameType)
            {
                case GameType.FourFours:
                    GameType = GameType.FourFours;
                    Description =
                        "Four fours is a mathematical puzzle. The goal of four fours is to find the simplest mathematical expression" +
                        " for every whole number from 0 to some maximum, using only common mathematical symbols and the digit four." +
                        " Only the following operations may be used: addition, subtraction, multiplication, division," +
                        " and exponents, modulus, and parentheses.  The original game allowed concatenation of any of the 4's," +
                        " but is not part of this implementation." +
                        " In the case of powers, your must use one or more of the 4s, or operations on the 4s, to form your exponent." +
                        "\r\nWhen an exact solution is not available, the closest solution is found. " +
                        "\r\nThe expression must have exactly four fours. No other digit is allowed. ";
                    DealButtonText = "Start Puzzle";
                    SolveButtonText = "Solve Next";
                    NumberOfOperands = 4;
                    useExponents = true;
                    useModulus = true;
                    findClosest = false;
                    RandomizerType = RandomizerType.Fours;
                break;
                case GameType.KryptoCards:
                    GameType = GameType.KryptoCards;
                    Description = 
                        "Krypto Cards is a Mathematical Card Game. Five playing cards are dealt." +
                        " Then, a sixed card is reveiled, and is called the Target Card." +
                        " You must add, subtract, multiply, or divide using each of the playing card numbers" +
                        " to obtain the final solution equal to the Target Card." +                       
                        " Fractions are not permitted in your calculation." +
                        "\r\nEach card must be used once and only once to obtain Target Card number. ";
                    DealButtonText = "Deal Cards";
                    SolveButtonText = "Solve Krypto";
                    NumberOfOperands = 5;
                    useExponents = false;
                    useModulus = false;
                    findClosest = false;
                    RandomizerType = RandomizerType.Card;
                break;
                case GameType.MathDice:
                    GameType = GameType.MathDice;
                    Description = 
                        "Math Dice is a Dice Game. Three 6-Sided playing dice are rolled." +
                        " Then two 12-sided dice are rolled to determine the Target Number." +
                        " You must add, subtract, multiply, divide, or powers using each of the playing dice" +
                        " to obtain the final solution equal to come as close as possible to the Target Number." +
                        " Fractions, decimals, roots, or exponents are not permitted." +
                        " In the case of powers, you must use one or more of the playing dice to get your exponent." +
                        "\r\nAgain, to repeat, when an exact solution is not available, the closest solution is found. " +
                        "\r\nEach playing dice must be used once and only once to obtain the Target Number. ";
                    DealButtonText = "Roll Dice";
                    SolveButtonText = "Solve Math Dice";
                    NumberOfOperands = 3;
                    useExponents = false;
                    useModulus = false;
                    findClosest = true;
                    RandomizerType = RandomizerType.Dice;
                    break;
            }  
        }

        public int getOperand()
        {
            int returnVal = 0;
            switch (RandomizerType)
            {
                case RandomizerType.Card:
                    returnVal = deck.DealCard();
                    break;
                case RandomizerType.Dice:
                    returnVal = random.Next(1, 7);   // creates a number between 1 and 6
                   break;
                case RandomizerType.Fours:
                    returnVal = 4;          // Always Four
                    break;
            }

            return returnVal;

        }
        public int getTarget()
        {
            int returnVal = 0;
            switch (RandomizerType)
            {
                case RandomizerType.Card:
                    returnVal = deck.DealCard();
                    break;
                case RandomizerType.Dice:
                    var dice1 = random.Next(1, 12);   // creates a number between 1 and 12
                    var dice2 = random.Next(1, 12);   // creates a number between 1 and 12
                    var addOrMult = random.Next(1, 3);  // one or two

                    if (addOrMult == 1)
                    {
                        returnVal = dice1 + dice2;
                    }
                    else
                    {
                        returnVal = dice1 * dice2;
                    }
                    break;
                case RandomizerType.Fours:
                    returnVal = 0;          // Start at 0
                    break;
            }

            return returnVal;

        }

    }

    public enum RandomizerType
    {
        Undefined,
        Fours,
        Dice,
        Card
    }
    public enum GameType
    {
        Undefined,
        KryptoCards,
        MathDice,
        FourFours
    }
}
