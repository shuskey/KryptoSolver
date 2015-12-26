namespace WindowsFormsApplication1
{
    using System;
    using System.Windows.Forms;

    /// <summary> Displays a wait cursor while an instance is in scope. </summary>
    public class WaitCursorContext : IDisposable
    {
        public Cursor PriorCursor { get; private set; }

        public WaitCursorContext()
        {
            PriorCursor = Cursor.Current;
            Cursor.Current = Cursors.AppStarting;
        }

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        protected virtual void Dispose(bool disposing)
        {
            if (disposing)
            {
                // free managed resources
                if (PriorCursor != null)
                {
                    Cursor.Current = PriorCursor;
                    PriorCursor = null;
                }
            }
        }
    }
}
