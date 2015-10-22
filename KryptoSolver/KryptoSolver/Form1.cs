using System;
using System.Collections.Generic;
using System.Windows.Forms;
using FSharpKrypto;

namespace WindowsFormsApplication1
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private void btnDealCards_Click(object sender, EventArgs e)
        {

            List<int> newCards = KryptoController.DealNewCards(5);

            txtCard1.Text = newCards[0].ToString();
            txtCard2.Text = newCards[1].ToString();
            txtCard3.Text = newCards[2].ToString();
            txtCard4.Text = newCards[3].ToString();
            txtCard5.Text = newCards[4].ToString();

            txtResultCard.Text = newCards[5].ToString();

            txtKryptoSolution.Text = "";

            //textBox1.Text = Krypto.kryptoCardsAndResultString;
        }

        private void btnSolveKrypto_Click(object sender, EventArgs e)
        {
            List<int> kryptoCardsDelt = new List<int>();
            txtKryptoSolution.Text = "Thinking...";
            //MessageBox.Show("Krypto Solver", "Here we go!");
            try
            {
                kryptoCardsDelt.Add(Convert.ToInt16(txtCard1.Text));
                kryptoCardsDelt.Add(Convert.ToInt16(txtCard2.Text));
                kryptoCardsDelt.Add(Convert.ToInt16(txtCard3.Text));
                kryptoCardsDelt.Add(Convert.ToInt16(txtCard4.Text));
                kryptoCardsDelt.Add(Convert.ToInt16(txtCard5.Text));
                kryptoCardsDelt.Add(Convert.ToInt16(txtResultCard.Text));
                
                txtKryptoSolution.Text = Tree.kryptoSolutionWithTheseCards(kryptoCardsDelt);
            }
            catch (Exception)
            {

                txtKryptoSolution.Text =
                    "Invalid Input.  Please insure that the Krypto Cards are all integer numbers and try again.";
            }

        }

        private void textBox1_TextChanged(object sender, EventArgs e)
        {

        }
    }
}
