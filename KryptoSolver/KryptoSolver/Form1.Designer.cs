namespace WindowsFormsApplication1
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
            this.groupBox1.SuspendLayout();
            this.SuspendLayout();
            // 
            // txtCard1
            // 
            this.txtCard1.Location = new System.Drawing.Point(46, 17);
            this.txtCard1.Name = "txtCard1";
            this.txtCard1.Size = new System.Drawing.Size(34, 20);
            this.txtCard1.TabIndex = 1;
            this.txtCard1.TextChanged += new System.EventHandler(this.textBox1_TextChanged);
            // 
            // btnDealCards
            // 
            this.btnDealCards.Location = new System.Drawing.Point(12, 12);
            this.btnDealCards.Name = "btnDealCards";
            this.btnDealCards.Size = new System.Drawing.Size(80, 30);
            this.btnDealCards.TabIndex = 0;
            this.btnDealCards.Text = "Deal Cards";
            this.btnDealCards.UseVisualStyleBackColor = true;
            this.btnDealCards.Click += new System.EventHandler(this.btnDealCards_Click);
            // 
            // btnSolveKrypto
            // 
            this.btnSolveKrypto.Location = new System.Drawing.Point(12, 94);
            this.btnSolveKrypto.Name = "btnSolveKrypto";
            this.btnSolveKrypto.Size = new System.Drawing.Size(79, 28);
            this.btnSolveKrypto.TabIndex = 7;
            this.btnSolveKrypto.Text = "Solve Krypto";
            this.btnSolveKrypto.UseVisualStyleBackColor = true;
            this.btnSolveKrypto.Click += new System.EventHandler(this.btnSolveKrypto_Click);
            // 
            // txtKryptoSolution
            // 
            this.txtKryptoSolution.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.txtKryptoSolution.Location = new System.Drawing.Point(13, 128);
            this.txtKryptoSolution.Multiline = true;
            this.txtKryptoSolution.Name = "txtKryptoSolution";
            this.txtKryptoSolution.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.txtKryptoSolution.Size = new System.Drawing.Size(730, 145);
            this.txtKryptoSolution.TabIndex = 8;
            // 
            // txtCard2
            // 
            this.txtCard2.Location = new System.Drawing.Point(86, 17);
            this.txtCard2.Name = "txtCard2";
            this.txtCard2.Size = new System.Drawing.Size(34, 20);
            this.txtCard2.TabIndex = 2;
            // 
            // txtCard3
            // 
            this.txtCard3.Location = new System.Drawing.Point(126, 17);
            this.txtCard3.Name = "txtCard3";
            this.txtCard3.Size = new System.Drawing.Size(34, 20);
            this.txtCard3.TabIndex = 3;
            // 
            // txtCard4
            // 
            this.txtCard4.Location = new System.Drawing.Point(166, 17);
            this.txtCard4.Name = "txtCard4";
            this.txtCard4.Size = new System.Drawing.Size(35, 20);
            this.txtCard4.TabIndex = 4;
            // 
            // txtCard5
            // 
            this.txtCard5.Location = new System.Drawing.Point(207, 17);
            this.txtCard5.Name = "txtCard5";
            this.txtCard5.Size = new System.Drawing.Size(34, 20);
            this.txtCard5.TabIndex = 5;
            // 
            // txtResultCard
            // 
            this.txtResultCard.Location = new System.Drawing.Point(276, 17);
            this.txtResultCard.Name = "txtResultCard";
            this.txtResultCard.Size = new System.Drawing.Size(34, 20);
            this.txtResultCard.TabIndex = 6;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label1.Location = new System.Drawing.Point(250, 17);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(19, 20);
            this.label1.TabIndex = 9;
            this.label1.Text = "=";
            // 
            // groupBox1
            // 
            this.groupBox1.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.groupBox1.Controls.Add(this.label1);
            this.groupBox1.Controls.Add(this.txtResultCard);
            this.groupBox1.Controls.Add(this.txtCard5);
            this.groupBox1.Controls.Add(this.txtCard4);
            this.groupBox1.Controls.Add(this.txtCard3);
            this.groupBox1.Controls.Add(this.txtCard2);
            this.groupBox1.Controls.Add(this.txtCard1);
            this.groupBox1.Location = new System.Drawing.Point(199, 54);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(325, 49);
            this.groupBox1.TabIndex = 10;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Krypto Cards";
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(755, 285);
            this.Controls.Add(this.groupBox1);
            this.Controls.Add(this.txtKryptoSolution);
            this.Controls.Add(this.btnSolveKrypto);
            this.Controls.Add(this.btnDealCards);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "Form1";
            this.Text = "Krypto Solver - In F#";
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
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
    }
}

