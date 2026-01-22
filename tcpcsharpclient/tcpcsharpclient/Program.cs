using System;
using System.Net.Sockets;
using System.Text;

namespace tcpcsharpclient
{
    internal class Program
    {
        static void Main()
        {
            try
            {
                // Connect to server
                using (TcpClient client = new TcpClient("127.0.0.1", 8000))
                using (NetworkStream stream = client.GetStream())
                {
                    while (true)
                    {
                        // Send message
                        string message = "play_card 5\n";
                        byte[] data = Encoding.ASCII.GetBytes(message);
                        stream.Write(data, 0, data.Length);

                        // Receive response
                        byte[] buffer = new byte[512];
                        int bytes = stream.Read(buffer, 0, buffer.Length);

                        if (bytes > 0)
                        {
                            string response = Encoding.ASCII.GetString(buffer, 0, bytes);
                            Console.WriteLine("Server says: " + response);
                        }
                        string t = Console.ReadLine();
                        if (t == "end")
                        {
                            break;
                        }
                    }
                    

                    const string GAME_OVER = "game_over";
                    byte[] data2 = Encoding.ASCII.GetBytes(GAME_OVER);
                    stream.Write(data2, 0, data2.Length);
                    Console.ReadLine();
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine("Error: " + ex.Message);
            }
        }
    }

}
