namespace Compiler;

using MyParser;

static class Program
{
    static void Main(string[] args)
    {
        if (args.Length != 1) throw new Exception("The program to execute has not been introduced");

        var programSrc = File.ReadAllText(args[0]);

        var program = Parser.mpParse(programSrc);

        Interpreter.mpRun(program);
    }
}