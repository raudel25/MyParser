namespace Compiler;

using MyParser;

static class Program
{
    static void Main(string[] args)
    {
        if (args.Length == 0)
        {
            Interactive();
            return;
        }

        if (!File.Exists(args[0]))
        {
            Console.WriteLine("File does not exist");
            return;
        }

        var programSrc = File.ReadAllText(args[0]);

        try
        {
            var program = Parser.mpParse(programSrc);

            Interpreter.mpRun(program);
        }
        catch (Exception e)
        {
            Console.WriteLine(e.Message);
        }
    }

    static void Interactive()
    {
        var program = Array.Empty<instruction>();
        var (stateVar, stateFunc) = Interpreter.mpState;
        var start = 0;

        while (true)
        {
            Console.Write(">> ");
            var instr = Console.ReadLine();

            if (instr is "q" or "quit" or "exit") break;

            try
            {
                var newProgram = Parser.mpParse(instr);

                program = program.Concat(newProgram).ToArray();

                start = Interpreter.mpInteractive(stateVar, stateFunc, program, start);
            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);

                program = Array.Empty<instruction>();
                stateVar.Clear();
                stateFunc.Clear();
                
                start = 0;
            }
        }
    }
}