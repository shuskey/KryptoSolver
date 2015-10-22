using System;
using System.Collections.Generic;
using FSharpKrypto;

namespace WindowsFormsApplication1
{
    class KryptoController
    {
        public static List<int> DealNewCards(int numberOfCards)
        {
            List<int> kryptoCardsDelt = new List<int>();

            Random random = new Random();

            var kryptoDeck =  Krypto.kryptoCardDeck;
            for (int i = 0; i < numberOfCards; i++)
            {
                kryptoCardsDelt.Add(Convert.ToInt16(Krypto.getNodeValue(kryptoDeck[random.Next(kryptoDeck.Length)])));
            }

            //one more for the desired result card
            kryptoCardsDelt.Add(Convert.ToInt16(Krypto.getNodeValue(kryptoDeck[random.Next(kryptoDeck.Length)])));

            return kryptoCardsDelt;

        }
    }
}
