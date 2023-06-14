namespace Compiler;

using PySharp;

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
            var program = PySharp.mpParse(programSrc);

            PySharp.mpRun(program);
        }
        catch (Exception e)
        {
            Console.WriteLine(e.Message);
        }
    }

    static bool Break(string? instr) => instr is "q" or "quit" or "exit";

    static void Interactive()
    {
        var (scopeVar, scopeFunc, scopeClass,scopeModules) = PySharp.mpScope;

        while (true)
        {
            Console.Write(">> ");
            var instr = Console.ReadLine();

            if (instr is "init") instr = Block();

            if (Break(instr))
                break;

            if (instr != null && scopeVar.ContainsKey(instr))
            {
                Console.WriteLine(PySharp.mpToStr(scopeVar[instr]));
                continue;
            }

            try
            {
                var program = PySharp.mpParse(instr);
                
                PySharp.mpInteractive(scopeVar, scopeFunc, scopeClass,scopeModules, program);
            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
                scopeVar.Clear();
                scopeFunc.Clear();
                scopeClass.Clear();
                scopeModules.Clear();
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