using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Threading;
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
            txtKryptoSolution.Clear();
            txtKryptoSolution.AppendText("Click the '" + CurrentGame.DealButtonText + "' Button to get started.");

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
            

            if (CurrentGameType == GameType.FourFours)
            {
                txtKryptoSolution.Clear();
                txtKryptoSolution.AppendText("Starting on first 'Four Fours' solution...");
                backgroundWorker1.RunWorkerAsync();
                //SolveKrypto();
            }
            else
            {
                txtKryptoSolution.Clear();
                txtKryptoSolution.AppendText("Now click the '" + CurrentGame.SolveButtonText + "' Button to have the computer solve this.");
            }


            //textBox1.Text = Krypto.kryptoCardsAndResultString;
        }

        private void btnSolveKrypto_Click(object sender, EventArgs e)
        {
            if (btnSolveKrypto.Text.Equals("Cancel") || backgroundWorker1.IsBusy)
            {
                Tree.cancelKryptoSolution(true);
                //WHEN THE CANCEL ACTUALLY HAPPENS DO THE FOLLOWING
                //if (backgroundWorker1.IsBusy) backgroundWorker1.CancelAsync();
                //EnableUI(true);
            }
            else
            {

                txtKryptoSolution.Clear();
                //txtKryptoSolution.Text = null;
                txtKryptoSolution.AppendText("Thinking...");

                try
                {
                    if (CurrentGameType == GameType.FourFours)
                        // This is a Solve Next operation so increment the Result Card before solving
                        txtResultCard.Text = (Convert.ToInt16(txtResultCard.Text) + 1).ToString();
                }
                catch (Exception)
                {
                    txtKryptoSolution.Clear();
                    txtKryptoSolution.AppendText(
                        "Invalid Input.  Please insure that the operands and target values are all integer numbers and try again." +
                        "\r\n\nTry clicking the '" + CurrentGame.DealButtonText + "' Button.");
                }

                EnableUI(false);
                // http://stuff.seans.com/2009/05/21/net-basics-do-work-in-background-thread-to-keep-gui-responsive/

                backgroundWorker1.RunWorkerAsync();
                //SolveKrypto();
            }
        }

        private void EnableUI(bool enable)
        {
            this.rBtnKryptoCards.Enabled = this.rBtnMathDice.Enabled = this.rBtnFourFours.Enabled = enable;
            this.cBoxExp.Enabled = this.cBoxMod.Enabled = this.cBoxClosest.Enabled = enable;
            this.txtUpDnNumberofOperands.Enabled = enable;
            this.txtCard1.Enabled = this.txtCard2.Enabled = this.txtCard3.Enabled =
                this.txtCard4.Enabled = this.txtCard5.Enabled = this.txtCard6.Enabled = enable;
            this.txtResultCard.Enabled = enable;
            this.btnDealCards.Enabled = enable;
            if (enable)
            {
                this.btnSolveKrypto.Text = CurrentGame.SolveButtonText;
            }
            else
            {
                this.btnSolveKrypto.Text = "Cancel";
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

        private void backgroundWorker1_DoWork(object sender, DoWorkEventArgs e)
        {

            List<int> kryptoCardsDelt = new List<int>();
            string sArgument = (string)e.Argument;

        
            kryptoStopwatch.Start();


            kryptoCardsDelt.Add(Convert.ToInt16(txtCard1.Text));
            kryptoCardsDelt.Add(Convert.ToInt16(txtCard2.Text));
            if (CurrentGame.NumberOfOperands > 2) kryptoCardsDelt.Add(Convert.ToInt16(txtCard3.Text));
            if (CurrentGame.NumberOfOperands > 3) kryptoCardsDelt.Add(Convert.ToInt16(txtCard4.Text));
            if (CurrentGame.NumberOfOperands > 4) kryptoCardsDelt.Add(Convert.ToInt16(txtCard5.Text));
            if (CurrentGame.NumberOfOperands > 5) kryptoCardsDelt.Add(Convert.ToInt16(txtCard6.Text));

            kryptoCardsDelt.Add(Convert.ToInt16(txtResultCard.Text));

            var tmpSolutionString = Tree.kryptoSolutionWithTheseCards(kryptoCardsDelt, CurrentGame.useExponents,
                CurrentGame.useModulus, CurrentGame.findClosest);

            //var tmpSolutionString = Tree.kryptoMain(kryptoCardsDelt, CurrentGame.useExponents,
            //    CurrentGame.useModulus, CurrentGame.findClosest);


            //for (int i = 0; i <= 100; i++)
            //{
            //    Thread.Sleep(100);
            //    if (backgroundWorker1.CancellationPending)
            //    {
            //        e.Cancel = true;
            //        return;
            //    }
            //}

            //var tmpSolutionString = "Success. just sleeping for 10 seconds";

            kryptoStopwatch.Stop();
            TimeSpan elapsedTime = kryptoStopwatch.Elapsed;
            string showTimeAs = String.Format("{0:00}:{1:00}:{2:00}.{3:00}", elapsedTime.Hours,
                elapsedTime.Minutes, elapsedTime.Seconds, elapsedTime.Milliseconds / 10);

            e.Result = tmpSolutionString + "\r\r\nElapsed Time: " + showTimeAs;

            kryptoStopwatch.Reset();

        }

        private void backgroundWorker1_RunWorkerCompleted(object sender, RunWorkerCompletedEventArgs e)
        {

            if (e.Error != null)
            {
                txtKryptoSolution.Clear();
                txtKryptoSolution.AppendText(e.Error.Message);
                //MessageBox.Show(e.Error.Message);
            }
            else
            {
                if (e.Cancelled)
                {
                    txtKryptoSolution.Clear();
                    txtKryptoSolution.AppendText("Processing Cancelled");
                }
                else
                {
                    txtKryptoSolution.Clear();
                    txtKryptoSolution.AppendText((string)e.Result);

                }
            }

            EnableUI(true);

        }

    }
}
