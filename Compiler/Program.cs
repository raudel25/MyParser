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

    static bool Break(string? instr) => instr is "q" or "quit" or "exit";

    static void Interactive()
    {
        var (stateVar, stateFunc, stateStruct) = Interpreter.mpState;

        while (true)
        {
            Console.Write(">> ");
            var instr = Console.ReadLine();

            if (instr is "init") instr = Block();

            if (Break(instr))
                break;

            if (instr != null && stateVar.ContainsKey(instr))
            {
                Console.WriteLine(LibraryFunc.toStr(stateVar[instr]));
                continue;
            }

            try
            {
                var program = Parser.mpParse(instr);

                Interpreter.mpInteractive(stateVar, stateFunc, stateStruct, program);
            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
                stateVar.Clear();
                stateFunc.Clear();
            }
        }
    }

    static string? Block()
    {
        var instr = "";
        while (true)
        {
            Console.Write(">> ");
            var newInstr = Console.ReadLine();

            if (newInstr is "end")
                return instr;

            if (Break(newInstr))
                return newInstr;

            instr = instr + newInstr + '\n';
        }
    }
}