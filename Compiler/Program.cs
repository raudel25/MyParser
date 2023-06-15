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
        
        var modules= Compiler(args[0]);

        if(modules is null) return;
        if (!modules.ContainsKey("main"))
        {
            Console.WriteLine("The file main.ps does not exist");
            return;
        }

        var main = modules["main"];
        modules.Remove("main");

        try
        {
            if (PySharp.mpCircularReference(modules))
            {
                Console.WriteLine("Circular dependency");
                return;
            }
            
            PySharp.mpRun(main,modules);
        }
        catch (Exception e)
        {
            Console.WriteLine(e.Message);
        }
    }

    static Dictionary<string,instruction[]>? Compiler(string path)
    {
        if (!Directory.Exists(path))
        {
            Console.WriteLine("Directory does not exist");
            return null;
        }
        
        var files = Directory.GetFiles(path, "*ps");

        var modules = PySharp.mpModules;

        foreach (var file in files)
        {
            var programSrc = File.ReadAllText(file);
            var module = Path.GetFileName(file)[..^3];
            try
            {
                var program = PySharp.mpParse(programSrc);
                
                modules.Add(module,program);
            }
            catch (Exception e)
            {
                Console.WriteLine($"File {module}\n{e.Message}");
                return null;
            }
        }
        
        return modules;
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