﻿namespace WindowsFormsApplication1
{
    partial class Form1
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Form1));
            this.txtCard1 = new System.Windows.Forms.TextBox();
            this.btnDealCards = new System.Windows.Forms.Button();
            this.btnSolveKrypto = new System.Windows.Forms.Button();
            this.txtKryptoSolution = new System.Windows.Forms.TextBox();
            this.txtCard2 = new System.Windows.Forms.TextBox();
            this.txtCard3 = new System.Windows.Forms.TextBox();
            this.txtCard4 = new System.Windows.Forms.TextBox();
            this.txtCard5 = new System.Windows.Forms.TextBox();
            this.txtResultCard = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.txtCard6 = new System.Windows.Forms.TextBox();
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.rBtnFourFours = new System.Windows.Forms.RadioButton();
            this.rBtnMathDice = new System.Windows.Forms.RadioButton();
            this.rBtnKryptoCards = new System.Windows.Forms.RadioButton();
            this.txtGameDescription = new System.Windows.Forms.TextBox();
            this.label2 = new System.Windows.Forms.Label();
            this.txtUpDnNumberofOperands = new System.Windows.Forms.NumericUpDown();
            this.cBoxExp = new System.Windows.Forms.CheckBox();
            this.cBoxMod = new System.Windows.Forms.CheckBox();
            this.toolTip1 = new System.Windows.Forms.ToolTip(this.components);
            this.cBoxClosest = new System.Windows.Forms.CheckBox();
            this.label3 = new System.Windows.Forms.Label();
            this.backgroundWorker1 = new System.ComponentModel.BackgroundWorker();
            this.groupBox1.SuspendLayout();
            this.groupBox2.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.txtUpDnNumberofOperands)).BeginInit();
            this.SuspendLayout();
            // 
            // txtCard1
            // 
            this.txtCard1.Font = new System.Drawing.Font("Consolas", 11.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtCard1.Location = new System.Drawing.Point(13, 17);
            this.txtCard1.Name = "txtCard1";
            this.txtCard1.Size = new System.Drawing.Size(34, 25);
            this.txtCard1.TabIndex = 1;
            // 
            // btnDealCards
            // 
            this.btnDealCards.Location = new System.Drawing.Point(10, 281);
            this.btnDealCards.Name = "btnDealCards";
            this.btnDealCards.Size = new System.Drawing.Size(104, 30);
            this.btnDealCards.TabIndex = 0;
            this.btnDealCards.Text = "Deal Cards";
            this.btnDealCards.UseVisualStyleBackColor = true;
            this.btnDealCards.Click += new System.EventHandler(this.btnDealCards_Click);
            // 
            // btnSolveKrypto
            // 
            this.btnSolveKrypto.Location = new System.Drawing.Point(479, 283);
            this.btnSolveKrypto.Name = "btnSolveKrypto";
            this.btnSolveKrypto.Size = new System.Drawing.Size(120, 28);
            this.btnSolveKrypto.TabIndex = 8;
            this.btnSolveKrypto.Text = "Solve Krypto";
            this.btnSolveKrypto.UseVisualStyleBackColor = true;
            this.btnSolveKrypto.Click += new System.EventHandler(this.btnSolveKrypto_Click);
            // 
            // txtKryptoSolution
            // 
            this.txtKryptoSolution.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.txtKryptoSolution.Font = new System.Drawing.Font("Consolas", 14.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtKryptoSolution.Location = new System.Drawing.Point(13, 326);
            this.txtKryptoSolution.Multiline = true;
            this.txtKryptoSolution.Name = "txtKryptoSolution";
            this.txtKryptoSolution.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.txtKryptoSolution.Size = new System.Drawing.Size(589, 183);
            this.txtKryptoSolution.TabIndex = 8;
            // 
            // txtCard2
            // 
            this.txtCard2.Font = new System.Drawing.Font("Consolas", 11.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtCard2.Location = new System.Drawing.Point(53, 17);
            this.txtCard2.Name = "txtCard2";
            this.txtCard2.Size = new System.Drawing.Size(34, 25);
            this.txtCard2.TabIndex = 2;
            // 
            // txtCard3
            // 
            this.txtCard3.Font = new System.Drawing.Font("Consolas", 11.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtCard3.Location = new System.Drawing.Point(93, 17);
            this.txtCard3.Name = "txtCard3";
            this.txtCard3.Size = new System.Drawing.Size(34, 25);
            this.txtCard3.TabIndex = 3;
            // 
            // txtCard4
            // 
            this.txtCard4.Font = new System.Drawing.Font("Consolas", 11.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtCard4.Location = new System.Drawing.Point(133, 17);
            this.txtCard4.Name = "txtCard4";
            this.txtCard4.Size = new System.Drawing.Size(34, 25);
            this.txtCard4.TabIndex = 4;
            // 
            // txtCard5
            // 
            this.txtCard5.Font = new System.Drawing.Font("Consolas", 11.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtCard5.Location = new System.Drawing.Point(173, 17);
            this.txtCard5.Name = "txtCard5";
            this.txtCard5.Size = new System.Drawing.Size(34, 25);
            this.txtCard5.TabIndex = 5;
            // 
            // txtResultCard
            // 
            this.txtResultCard.Font = new System.Drawing.Font("Consolas", 11.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtResultCard.Location = new System.Drawing.Point(281, 17);
            this.txtResultCard.Name = "txtResultCard";
            this.txtResultCard.Size = new System.Drawing.Size(34, 25);
            this.txtResultCard.TabIndex = 7;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Font = new System.Drawing.Font("Consolas", 11.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label1.Location = new System.Drawing.Point(252, 17);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(16, 18);
            this.label1.TabIndex = 9;
            this.label1.Text = "=";
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.txtCard6);
            this.groupBox1.Controls.Add(this.label1);
            this.groupBox1.Controls.Add(this.txtResultCard);
            this.groupBox1.Controls.Add(this.txtCard5);
            this.groupBox1.Controls.Add(this.txtCard4);
            this.groupBox1.Controls.Add(this.txtCard3);
            this.groupBox1.Controls.Add(this.txtCard2);
            this.groupBox1.Controls.Add(this.txtCard1);
            this.groupBox1.Location = new System.Drawing.Point(137, 271);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(336, 49);
            this.groupBox1.TabIndex = 10;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Krypto Cards";
            // 
            // txtCard6
            // 
            this.txtCard6.Font = new System.Drawing.Font("Consolas", 11.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtCard6.Location = new System.Drawing.Point(213, 17);
            this.txtCard6.Name = "txtCard6";
            this.txtCard6.Size = new System.Drawing.Size(34, 25);
            this.txtCard6.TabIndex = 6;
            // 
            // groupBox2
            // 
            this.groupBox2.Controls.Add(this.rBtnFourFours);
            this.groupBox2.Controls.Add(this.rBtnMathDice);
            this.groupBox2.Controls.Add(this.rBtnKryptoCards);
            this.groupBox2.Location = new System.Drawing.Point(12, 12);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(119, 100);
            this.groupBox2.TabIndex = 9;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "Game Style";
            // 
            // rBtnFourFours
            // 
            this.rBtnFourFours.AutoSize = true;
            this.rBtnFourFours.Location = new System.Drawing.Point(18, 70);
            this.rBtnFourFours.Name = "rBtnFourFours";
            this.rBtnFourFours.Size = new System.Drawing.Size(75, 17);
            this.rBtnFourFours.TabIndex = 14;
            this.rBtnFourFours.Text = "Four Fours";
            this.rBtnFourFours.UseVisualStyleBackColor = true;
            this.rBtnFourFours.CheckedChanged += new System.EventHandler(this.rBtnFourFOurs_CheckedChanged);
            // 
            // rBtnMathDice
            // 
            this.rBtnMathDice.AutoSize = true;
            this.rBtnMathDice.Location = new System.Drawing.Point(18, 47);
            this.rBtnMathDice.Name = "rBtnMathDice";
            this.rBtnMathDice.Size = new System.Drawing.Size(74, 17);
            this.rBtnMathDice.TabIndex = 13;
            this.rBtnMathDice.Text = "Math Dice";
            this.rBtnMathDice.UseVisualStyleBackColor = true;
            this.rBtnMathDice.CheckedChanged += new System.EventHandler(this.RBtnMathDiceCheckedChanged);
            // 
            // rBtnKryptoCards
            // 
            this.rBtnKryptoCards.AutoSize = true;
            this.rBtnKryptoCards.Checked = true;
            this.rBtnKryptoCards.Location = new System.Drawing.Point(18, 24);
            this.rBtnKryptoCards.Name = "rBtnKryptoCards";
            this.rBtnKryptoCards.Size = new System.Drawing.Size(85, 17);
            this.rBtnKryptoCards.TabIndex = 12;
            this.rBtnKryptoCards.TabStop = true;
            this.rBtnKryptoCards.Text = "Krypto Cards";
            this.rBtnKryptoCards.UseVisualStyleBackColor = true;
            this.rBtnKryptoCards.CheckedChanged += new System.EventHandler(this.rBtnKryptoCards_CheckedChanged);
            // 
            // txtGameDescription
            // 
            this.txtGameDescription.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.txtGameDescription.BackColor = System.Drawing.SystemColors.Info;
            this.txtGameDescription.Font = new System.Drawing.Font("Microsoft YaHei UI", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtGameDescription.Location = new System.Drawing.Point(137, 19);
            this.txtGameDescription.Multiline = true;
            this.txtGameDescription.Name = "txtGameDescription";
            this.txtGameDescription.Size = new System.Drawing.Size(465, 246);
            this.txtGameDescription.TabIndex = 13;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(12, 227);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(50, 13);
            this.label2.TabIndex = 15;
            this.label2.Text = "Numer of";
            this.label2.Click += new System.EventHandler(this.label2_Click);
            // 
            // txtUpDnNumberofOperands
            // 
            this.txtUpDnNumberofOperands.Font = new System.Drawing.Font("Consolas", 11.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtUpDnNumberofOperands.Location = new System.Drawing.Point(86, 230);
            this.txtUpDnNumberofOperands.Maximum = new decimal(new int[] {
            5,
            0,
            0,
            0});
            this.txtUpDnNumberofOperands.Minimum = new decimal(new int[] {
            2,
            0,
            0,
            0});
            this.txtUpDnNumberofOperands.Name = "txtUpDnNumberofOperands";
            this.txtUpDnNumberofOperands.Size = new System.Drawing.Size(33, 25);
            this.txtUpDnNumberofOperands.TabIndex = 13;
            this.txtUpDnNumberofOperands.Value = new decimal(new int[] {
            5,
            0,
            0,
            0});
            this.txtUpDnNumberofOperands.ValueChanged += new System.EventHandler(this.txtUpDnNumberofOperands_ValueChanged);
            // 
            // cBoxExp
            // 
            this.cBoxExp.AutoSize = true;
            this.cBoxExp.Location = new System.Drawing.Point(15, 135);
            this.cBoxExp.Name = "cBoxExp";
            this.cBoxExp.Size = new System.Drawing.Size(107, 17);
            this.cBoxExp.TabIndex = 10;
            this.cBoxExp.Text = "Use ^ Exponents";
            this.toolTip1.SetToolTip(this.cBoxExp, "Exponent is how many times the number will be multiplied by itself.  4^3 = 4 x 4 " +
        "x 4 = 64.");
            this.cBoxExp.UseVisualStyleBackColor = true;
            this.cBoxExp.CheckedChanged += new System.EventHandler(this.cBoxExp_CheckedChanged);
            // 
            // cBoxMod
            // 
            this.cBoxMod.AutoSize = true;
            this.cBoxMod.Location = new System.Drawing.Point(15, 158);
            this.cBoxMod.Name = "cBoxMod";
            this.cBoxMod.Size = new System.Drawing.Size(99, 17);
            this.cBoxMod.TabIndex = 11;
            this.cBoxMod.Text = "Use % Modulus";
            this.toolTip1.SetToolTip(this.cBoxMod, "Modulus is the remainder after doing a division. 17 % 3 = 2 because 17 / 3 has a " +
        "remainder of 2.");
            this.cBoxMod.UseVisualStyleBackColor = true;
            this.cBoxMod.CheckedChanged += new System.EventHandler(this.cBoxMod_CheckedChanged);
            // 
            // toolTip1
            // 
            this.toolTip1.Popup += new System.Windows.Forms.PopupEventHandler(this.toolTip1_Popup);
            // 
            // cBoxClosest
            // 
            this.cBoxClosest.AutoSize = true;
            this.cBoxClosest.Location = new System.Drawing.Point(15, 181);
            this.cBoxClosest.Name = "cBoxClosest";
            this.cBoxClosest.Size = new System.Drawing.Size(83, 17);
            this.cBoxClosest.TabIndex = 12;
            this.cBoxClosest.Text = "Find Closest";
            this.toolTip1.SetToolTip(this.cBoxClosest, "If there is no exact solution, find the closest.  This may make the time to solve" +
        " up to 3 times slower.");
            this.cBoxClosest.UseVisualStyleBackColor = true;
            this.cBoxClosest.CheckedChanged += new System.EventHandler(this.cBoxClosest_CheckedChanged);
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(12, 242);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(53, 13);
            this.label3.TabIndex = 20;
            this.label3.Text = "Operands";
            // 
            // backgroundWorker1
            // 
            this.backgroundWorker1.WorkerSupportsCancellation = true;
            this.backgroundWorker1.DoWork += new System.ComponentModel.DoWorkEventHandler(this.backgroundWorker1_DoWork);
            this.backgroundWorker1.RunWorkerCompleted += new System.ComponentModel.RunWorkerCompletedEventHandler(this.backgroundWorker1_RunWorkerCompleted);
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(611, 521);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.cBoxClosest);
            this.Controls.Add(this.cBoxMod);
            this.Controls.Add(this.cBoxExp);
            this.Controls.Add(this.txtUpDnNumberofOperands);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.txtGameDescription);
            this.Controls.Add(this.groupBox1);
            this.Controls.Add(this.txtKryptoSolution);
            this.Controls.Add(this.btnSolveKrypto);
            this.Controls.Add(this.btnDealCards);
            this.Controls.Add(this.groupBox2);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "Form1";
            this.Text = "Krypto Solver - In F#";
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.groupBox2.ResumeLayout(false);
            this.groupBox2.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.txtUpDnNumberofOperands)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button btnDealCards;
        private System.Windows.Forms.Button btnSolveKrypto;
        public System.Windows.Forms.TextBox txtKryptoSolution;
        public System.Windows.Forms.TextBox txtCard1;
        public System.Windows.Forms.TextBox txtCard2;
        public System.Windows.Forms.TextBox txtCard3;
        public System.Windows.Forms.TextBox txtCard4;
        public System.Windows.Forms.TextBox txtCard5;
        public System.Windows.Forms.TextBox txtResultCard;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.RadioButton rBtnFourFours;
        private System.Windows.Forms.RadioButton rBtnMathDice;
        private System.Windows.Forms.RadioButton rBtnKryptoCards;
        public System.Windows.Forms.TextBox txtGameDescription;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.NumericUpDown txtUpDnNumberofOperands;
        public System.Windows.Forms.TextBox txtCard6;
        private System.Windows.Forms.CheckBox cBoxExp;
        private System.Windows.Forms.CheckBox cBoxMod;
        private System.Windows.Forms.ToolTip toolTip1;
        private System.Windows.Forms.CheckBox cBoxClosest;
        private System.Windows.Forms.Label label3;
        private System.ComponentModel.BackgroundWorker backgroundWorker1;
    }
}

