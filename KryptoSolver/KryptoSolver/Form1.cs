using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Windows.Forms;
using FSharpKrypto;
using KryptoSolver;

namespace WindowsFormsApplication1
{
    public partial class Form1 : Form
    {
        public GameDescriptor KryptoCardGame = new GameDescriptor(GameType.KryptoCards);
        public GameDescriptor MathDiceGame = new GameDescriptor(GameType.MathDice);
        public GameDescriptor FourFoursGame = new GameDescriptor(GameType.FourFours);
        public GameType CurrentGameType = GameType.KryptoCards;
        Stopwatch kryptoStopwatch = new Stopwatch();

        public GameDescriptor CurrentGame
        {
            get
            {
                switch (CurrentGameType)
                {
                    case GameType.KryptoCards:
                        return KryptoCardGame;
                    case GameType.MathDice:
                        return MathDiceGame;
                    case GameType.FourFours:
                        return FourFoursGame;
                }
                return KryptoCardGame;
            }
            set
            {
                var currentGame = value;
                CurrentGameType = currentGame.GameType;
                txtGameDescription.Text = currentGame.Description;
                btnDealCards.Text = currentGame.DealButtonText;
                btnSolveKrypto.Text = currentGame.SolveButtonText;
                txtUpDnNumberofOperands.Text = currentGame.NumberOfOperands.ToString();
                UpdateTxtBoxes();
            }
        }

        public int changeNumberOfOperands(int numberOfOperands)
        {
            int retValSet = Math.Max((int)txtUpDnNumberofOperands.Minimum, numberOfOperands); //clamp to at least the minimum
            retValSet = Math.Min((int) txtUpDnNumberofOperands.Maximum, retValSet); //if too big, reduce to the Maximum
            CurrentGame.NumberOfOperands = retValSet;
            UpdateTxtBoxes();
            return retValSet;
        }

        public void UpdateTxtBoxes()
        {
            txtCard1.Text = txtCard2.Text = txtCard3.Text = txtCard4.Text = txtCard5.Text = txtCard6.Text = "";
            txtResultCard.Text = "";
            txtKryptoSolution.Text = "Click the '" + CurrentGame.DealButtonText + "' Button to get started.";

            txtCard3.Visible = (CurrentGame.NumberOfOperands > 2);
            txtCard4.Visible = (CurrentGame.NumberOfOperands > 3);
            txtCard5.Visible = (CurrentGame.NumberOfOperands > 4);
            txtCard6.Visible = (CurrentGame.NumberOfOperands > 5);
        }

        public Form1()
        {
            InitializeComponent();
            CurrentGame = KryptoCardGame;
        }

        private void btnDealCards_Click(object sender, EventArgs e)
        {

            var numberOfOperands = CurrentGame.NumberOfOperands;
            txtCard1.Text = CurrentGame.getOperand().ToString();
            txtCard2.Text = CurrentGame.getOperand().ToString();
            if (CurrentGame.NumberOfOperands > 2) txtCard3.Text = CurrentGame.getOperand().ToString();
            if (CurrentGame.NumberOfOperands > 3) txtCard4.Text = CurrentGame.getOperand().ToString();
            if (CurrentGame.NumberOfOperands > 4) txtCard5.Text = CurrentGame.getOperand().ToString();
            if (CurrentGame.NumberOfOperands > 5) txtCard6.Text = CurrentGame.getOperand().ToString();

            txtResultCard.Text = CurrentGame.getTarget().ToString();

            txtKryptoSolution.Text = "Now click the '" + CurrentGame.SolveButtonText + "' Button to have the computer solve this.";

            if (CurrentGameType == GameType.FourFours)
                SolveKrypto();

            //textBox1.Text = Krypto.kryptoCardsAndResultString;
        }

        private void btnSolveKrypto_Click(object sender, EventArgs e)
        {
            try
            {
                if (CurrentGameType == GameType.FourFours) // This is a Solve Next operation so increment the Result Card before solving
                    txtResultCard.Text = (Convert.ToInt16(txtResultCard.Text) + 1).ToString();
            }
            catch (Exception)
            {
                txtKryptoSolution.Text =
                    "Invalid Input.  Please insure that the operands and target values are all integer numbers and try again." +
                    "\r\n\nTry clicking the '" + CurrentGame.DealButtonText + "' Button." ;
            }

            SolveKrypto();
        }

        private void SolveKrypto()
        {
            List<int> kryptoCardsDelt = new List<int>();
            

            kryptoStopwatch.Start();

            txtKryptoSolution.Text = "Thinking...";
            //MessageBox.Show("Krypto Solver", "Here we go!");
            try
            {
                kryptoCardsDelt.Add(Convert.ToInt16(txtCard1.Text));
                kryptoCardsDelt.Add(Convert.ToInt16(txtCard2.Text));
                if (CurrentGame.NumberOfOperands > 2) kryptoCardsDelt.Add(Convert.ToInt16(txtCard3.Text));
                if (CurrentGame.NumberOfOperands > 3) kryptoCardsDelt.Add(Convert.ToInt16(txtCard4.Text));
                if (CurrentGame.NumberOfOperands > 4) kryptoCardsDelt.Add(Convert.ToInt16(txtCard5.Text));
                if (CurrentGame.NumberOfOperands > 5) kryptoCardsDelt.Add(Convert.ToInt16(txtCard6.Text));

                kryptoCardsDelt.Add(Convert.ToInt16(txtResultCard.Text));

                txtKryptoSolution.Text = Tree.kryptoSolutionWithTheseCards(kryptoCardsDelt, CurrentGame.useExponents, CurrentGame.useModulus, CurrentGame.findClosest);



                kryptoStopwatch.Stop();
                TimeSpan elapsedTime = kryptoStopwatch.Elapsed;
                string showTimeAs = String.Format("{0:00}:{1:00}:{2:00}.{3:00}", elapsedTime.Hours, elapsedTime.Minutes, elapsedTime.Seconds, elapsedTime.Milliseconds / 10);

                txtKryptoSolution.Text += "\r\r\nElapsed Time: " + showTimeAs;
                
                kryptoStopwatch.Reset();
            }
            catch (Exception)
            {

                txtKryptoSolution.Text =
                    "Invalid Input.  Please insure that the operands and target values are all integer numbers and try again."+
                    "\r\n\nTry clicking the '" + CurrentGame.DealButtonText + "' Button.";

            }

        }

        private void RBtnMathDiceCheckedChanged(object sender, EventArgs e)
        {
            if (rBtnMathDice.Checked)
            {
                CurrentGame = MathDiceGame;
                cBoxExp.Checked = CurrentGame.useExponents;
                cBoxMod.Checked = CurrentGame.useModulus;
                cBoxClosest.Checked = CurrentGame.findClosest;
            }

        }

        private void rBtnFourFOurs_CheckedChanged(object sender, EventArgs e)
        {
            if (rBtnFourFours.Checked)
            { 
                CurrentGame = FourFoursGame;
                cBoxExp.Checked = CurrentGame.useExponents;
                cBoxMod.Checked = CurrentGame.useModulus;
                cBoxClosest.Checked = CurrentGame.findClosest;
            }
        }

        private void rBtnKryptoCards_CheckedChanged(object sender, EventArgs e)
        {
            if (rBtnKryptoCards.Checked)
            {
                CurrentGame = KryptoCardGame;
                cBoxExp.Checked = CurrentGame.useExponents;
                cBoxMod.Checked = CurrentGame.useModulus;
                cBoxClosest.Checked = CurrentGame.findClosest;
            }

        }

        private void txtUpDnNumberofOperands_ValueChanged(object sender, EventArgs e)
        {
            var clampedVal = changeNumberOfOperands((int) txtUpDnNumberofOperands.Value);
        }

        private void toolTip1_Popup(object sender, PopupEventArgs e)
        {

        }

        private void label2_Click(object sender, EventArgs e)
        {

        }

        private void cBoxExp_CheckedChanged(object sender, EventArgs e)
        {
            CurrentGame.useExponents = cBoxExp.Checked;
        }

        private void cBoxMod_CheckedChanged(object sender, EventArgs e)
        {
            CurrentGame.useModulus = cBoxMod.Checked;
        }

        private void cBoxClosest_CheckedChanged(object sender, EventArgs e)
        {
            CurrentGame.findClosest = cBoxClosest.Checked;
        }
    }
}
