using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace KryptoSolver
{
    class CardDeck
    {
        private List<int> _cards = new List<int>();
        private Random _random = new Random();

        public CardDeck()
        {
            Helper_AddToCardDeck(_cards, 3, 1, 10);
            Helper_AddToCardDeck(_cards, 2, 11, 17);
            Helper_AddToCardDeck(_cards, 1, 18, 25);
        }

        public int DealCard()
        {
            return _cards[_random.Next(_cards.Count)];
        }

        /// <summary>
        /// Helps Stack the deck with repeating patterns
        /// For example a Krypto Deck needs: 
        /// 3 sets of cards valued 1 thru 6 and
        /// 4 sets of cards valued 7 thru 10 and
        /// 2 sets of cards valued 11 thru 17 and
        /// 1 set of cards valued 18 thru 25
        /// </summary>
        public void Helper_AddToCardDeck(List<int> myDeck, int repeat, int startVal, int endVal)
        {
            for (int r = 0; r < repeat; r++)
            {
                for (int cardNumber = startVal; cardNumber <= endVal; cardNumber++)
                {
                    myDeck.Add(cardNumber);
                    
                }
                
            }
     
    
        }
    }


}
