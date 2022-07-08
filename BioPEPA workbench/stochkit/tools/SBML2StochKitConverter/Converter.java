// Decompiled by Jad v1.5.8e. Copyright 2001 Pavel Kouznetsov.
// Jad home page: http://www.geocities.com/kpdus/jad.html
// Decompiler options: packimports(3) 
// Source File Name:   Converter.java

import XML.SBMLFileReader;
import XML.SBMLObject;
import java.io.*;
import java.util.Stack;
import javax.swing.JOptionPane;

public class Converter
{

    public Converter(String s, String s1)
    {
        SJNIL2 sjnil2 = new SJNIL2();
        SBMLFileReader sbmlfilereader = new SBMLFileReader(s);
        SBMLObject sbmlobject = sbmlfilereader.readObject();
        String s2 = sbmlobject.getAttribute("version");
        String s3 = sbmlobject.getAttribute("level");
        String s4 = "";
        if(s2.equals("1"))
        {
            if(s3.equals("1"))
                s4 = "1";
            else
            if(s3.equals("2"))
                s4 = "2";
        } else
        if(s2.equals("2"))
            s4 = "2";
        String s5 = "";
        SBMLObject sbmlobject1 = sbmlobject.getChild("model");
        if(s4.equals("1"))
            s5 = sbmlobject1.getAttribute("name");
        else
        if(s4.equals("2"))
            s5 = sbmlobject1.getAttribute("id");
        SBMLObject asbmlobject[] = sbmlobject1.getChildren("listOfSpecies");
        SBMLObject asbmlobject1[] = asbmlobject[0].getChildren("specie");
        numspecies = asbmlobject1.length;
        String s6 = "";
        String as[] = new String[numspecies];
        String s7 = "";
        species_array = new String[numspecies];
        String s8 = "Vector Initialize()\n{\n ";
        s8 = s8 + "   Vector x0(";
        s8 = s8 + numspecies + ", 0.0);\n\n";
        String s9 = "";
        for(int i = 0; i < numspecies; i++)
        {
            if(s4.equals("1"))
            {
                s6 = s6 + asbmlobject1[i].getAttribute("name") + "\n";
                species_array[i] = asbmlobject1[i].getAttribute("name");
                s9 = asbmlobject1[i].getAttribute("initialAmount");
            } else
            if(s4.equals("2"))
            {
                s6 = s6 + asbmlobject1[i].getAttribute("id") + "\n";
                species_array[i] = asbmlobject1[i].getAttribute("id");
                s9 = asbmlobject1[i].getAttribute("initialConcentration");
            }
            as[i] = s9;
            if(!s9.equals("0"))
                s8 = s8 + "    x0(" + i + ") = " + s9 + ";\n";
        }

        s8 = s8 + "\n    return x0;\n} \n\n";
        SBMLObject asbmlobject2[] = sbmlobject1.getChildren("listOfReactions");
        SBMLObject asbmlobject3[] = asbmlobject2[0].getChildren("reaction");
        int j1 = asbmlobject3.length;
        s7 = numspecies + " " + j1 + "\n";
        String s10 = "#include \"ProblemDefinition.h\"\n\n";
        String s11 = "Matrix Stoichiometry()\n{\n";
        String s12 = "Matrix DependentGrapth()\n{\n";
        String s13 = "Vector Propensity(const Vector& x)\n{\n    double\n";
        String s14 = "Vector PartialProp(int RIndex, const Vector& x, const Matrix& dg, Vector& a, double& a0)\n{\n    double\n";
        String s15 = "Matrix PropensityJacobian(const Vector& x)\n{\n    double\n";
        String s16 = "";
        String s17 = "";
        String s18 = "";
        String s19 = "void Equilibrium(Vector& x, Vector& a, int rxn)\n{\n      }\n";
        String s20 = "";
        String s21 = "";
        String s23 = "";
        String s27 = "";
        boolean flag = false;
        int ai3[] = new int[numspecies];
        String as1[] = new String[j1];
        String as2[] = new String[j1];
        String as3[] = new String[j1];
        String s31 = "";
        int ai[][] = new int[j1 + 2][numspecies + 2];
        int ai1[][] = new int[j1 + 2][numspecies + 2];
        int ai2[][] = new int[j1 + 2][j1 + 2];
        for(int k1 = 0; k1 <= j1; k1++)
        {
            for(int j = 0; j <= numspecies; j++)
            {
                ai[k1][j] = 0;
                ai1[k1][j] = 0;
            }

        }

        for(int l1 = 0; l1 <= j1; l1++)
        {
            for(int k = 0; k <= j1; k++)
                ai2[l1][k] = 0;

        }

        s17 = s17 + "    Vector a(" + j1 + ", 0.0);\n\n";
        s18 = s18 + "\n" + "    int i=0, tempI=0;\n" + "    while(dg(RIndex, i) != -1){\n" + "         tempI = int(dg(RIndex, i));\n" + "         i++;\n" + "         switch(tempI)\n" + "         {\n";
        s20 = s20 + "    Matrix j(" + j1 + "," + numspecies + ",0.0);\n\n";
        s11 = s11 + "    Matrix nu(" + numspecies + ", " + j1 + ", 0.0);\n\n";
        int i2 = j1 + 1;
        s12 = s12 + "    Matrix DG(" + i2 + ", " + i2 + ", 0.0);\n\n";
        Stack stack = new Stack();
        for(int j2 = 0; j2 < j1; j2++)
        {
            System.out.println("---------------------------");
            System.out.println("examining reaction " + j2);
            System.out.println("---------------------------");
            as2[j2] = "";
            as3[j2] = "";
            SBMLObject asbmlobject4[] = asbmlobject3[j2].getChildren("listOfReactants");
            SBMLObject asbmlobject5[] = asbmlobject4[0].getChildren("specieReference");
            SBMLObject asbmlobject6[] = asbmlobject3[j2].getChildren("listOfProducts");
            SBMLObject asbmlobject7[] = asbmlobject6[0].getChildren("specieReference");
            SBMLObject sbmlobject2 = asbmlobject3[j2].getChild("kineticLaw");
            SBMLObject sbmlobject3 = sbmlobject2.getChild("listOfParameters");
            System.out.println("--before-------------------------");
            SBMLObject asbmlobject8[] = sbmlobject3.getChildren("parameter");
            System.out.println("--after-------------------------");
            int i4 = asbmlobject7.length;
            for(int l3 = 0; l3 < asbmlobject8.length; l3++)
            {
                if(s4.equals("1"))
                    s31 = asbmlobject8[l3].getAttribute("name");
                else
                if(s4.equals("2"))
                    s31 = asbmlobject8[l3].getAttribute("id");
                if(stack.search(s31) == -1)
                {
                    if(!stack.empty())
                        s16 = s16 + ",\n";
                    as1[j2] = s31 + "=" + asbmlobject8[l3].getAttribute("value");
                    s16 = s16 + "    " + as1[j2];
                    stack.push(s31);
                }
            }

            String s34 = sbmlobject2.getAttribute("formula");
            String s22 = s34;
            for(int k5 = 0; k5 < numspecies; k5++)
                s34 = sjnil2.execute("perl ./perl/replaceAll.perl \"" + s34 + "\" " + species_array[k5] + " " + "x\\" + "(" + k5 + "\\" + ")");

            s17 = s17 + "    a(" + j2 + ") = " + s34 + ";\n";
            s18 = s18 + "             case " + j2 + ":\n" + "             {\n" + "                  a0 = a0 -a(" + j2 + ");\n" + "                  a(" + j2 + ") = " + s34 + ";\n" + "                  a0 = a0 + a(" + j2 + ");\n" + "             }\n";
            int k4 = 0;
            for(int l5 = 0; l5 < numspecies; l5++)
                ai3[l5] = 0;

            int j4 = 0;
            String s24 = s22;
            for(int i6 = 0; i6 < numspecies; i6++)
            {
                String s28 = sjnil2.execute("perl ./perl/TestXIndependent.perl \"" + s24 + "\" " + species_array[i6]);
                if(s28.equals("Y"))
                {
                    k4++;
                    ai3[j4] = i6;
                    j4++;
                }
            }

            String s29 = "";
            j4 = 0;
            if(k4 != 0)
                if(k4 == 1)
                {
                    String s30 = sjnil2.execute("perl ./perl/TestSecX.perl \"" + s24 + "\" " + species_array[ai3[j4]]);
                    if(s30.equals("Y"))
                    {
                        String s25 = s22;
                        s25 = sjnil2.execute("perl ./perl/ReplaceXminusOne.perl \"" + s25 + "\" " + species_array[ai3[j4]]);
                        s22 = s25 + "*2-";
                        s25 = sjnil2.execute("perl ./perl/GenerateJacoForOneX.perl \"" + s25 + "\" " + species_array[ai3[j4]]);
                        s22 = s22 + s25;
                        s22 = sjnil2.execute("perl ./perl/replaceAll.perl \"" + s22 + "\" " + species_array[ai3[j4]] + " " + "x\\" + "(" + ai3[j4] + "\\" + ")");
                        s20 = s20 + "    j(" + j2 + "," + ai3[j4] + ") = " + s22 + ";\n";
                    } else
                    {
                        s22 = sjnil2.execute("perl ./perl/GenerateJacoForOneX.perl \"" + s22 + "\" " + species_array[ai3[j4]]);
                        s20 = s20 + "    j(" + j2 + "," + ai3[j4] + ") = " + s22 + ";\n";
                    }
                } else
                if(k4 == 2)
                {
                    for(int j6 = 0; j6 < k4; j6++)
                    {
                        String s26 = s22;
                        s26 = sjnil2.execute("perl ./perl/GenerateJacoForOneX.perl \"" + s26 + "\" " + species_array[ai3[j4]]);
                        for(int i7 = 0; i7 < k4; i7++)
                            s26 = sjnil2.execute("perl ./perl/replaceAll.perl \"" + s26 + "\" " + species_array[ai3[i7]] + " " + "x\\" + "(" + ai3[i7] + "\\" + ")");

                        s20 = s20 + "   j(" + j2 + "," + ai3[j4] + ") = " + s26 + ";\n";
                        j4++;
                    }

                }
            boolean flag7 = false;
            boolean flag9 = false;
            boolean flag11 = false;
            String s35 = "";
            String s37 = "";
            String s39 = "";
            String s44 = "";
            int j8 = 1;
            boolean flag6 = false;
            for(int j7 = 0; j7 < asbmlobject5.length; j7++)
            {
                if(j7 != 0)
                    as2[j2] = as2[j2] + "+";
                if(asbmlobject5[j7].getAttribute("stoichiometry") != null)
                    s35 = asbmlobject5[j7].getAttribute("stoichiometry");
                String s40 = asbmlobject5[j7].getAttribute("specie");
                String s45 = s40;
                for(int k6 = 0; k6 < numspecies; k6++)
                    s45 = sjnil2.execute("perl ./perl/replaceAll.perl \"" + s45 + "\" " + species_array[k6] + " " + k6);

                int l8 = Integer.parseInt(s45) + 1;
                for(int j9 = 1; ai1[j2 + 1][j9] != 0; j9++)
                {
                    if(ai1[j2 + 1][j9] != l8)
                        continue;
                    flag6 = true;
                    break;
                }

                if(!flag6)
                {
                    ai1[j2 + 1][j8] = l8;
                    ai[j2 + 1][j8] = l8;
                    flag6 = false;
                    j8++;
                }
                as2[j2] = as2[j2] + s40;
                if(s35.equals("2"))
                    as2[j2] = as2[j2] + "+" + asbmlobject5[j7].getAttribute("specie");
                if(s35.equals("3"))
                    as2[j2] = as2[j2] + "+" + asbmlobject5[j7].getAttribute("specie") + "+" + asbmlobject5[j7].getAttribute("specie");
            }

            int k8 = 1;
            flag6 = false;
            for(int l7 = 0; l7 < i4; l7++)
            {
                if(l7 != 0)
                    as3[j2] = as3[j2] + "+";
                if(asbmlobject7[l7].getAttribute("stoichiometry") != null)
                    s35 = asbmlobject7[l7].getAttribute("stoichiometry");
                String s41 = asbmlobject7[l7].getAttribute("specie");
                as3[j2] = as3[j2] + s41;
                String s46 = s41;
                for(int l6 = 0; l6 < numspecies; l6++)
                    s46 = sjnil2.execute("perl ./perl/replaceAll.perl \"" + s46 + "\" " + species_array[l6] + " " + l6);

                int i9 = Integer.parseInt(s46) + 1;
                for(int k9 = 1; ai[j2 + 1][k9] != 0; k9++)
                {
                    if(ai[j2 + 1][k9] != i9)
                        continue;
                    flag6 = true;
                    break;
                }

                if(!flag6)
                {
                    ai[j2 + 1][(j8 - 1) + k8] = i9;
                    flag6 = false;
                    k8++;
                }
                if(s35.equals("2"))
                    as3[j2] = as3[j2] + "+" + asbmlobject7[l7].getAttribute("specie");
                if(s35.equals("3"))
                    as3[j2] = as3[j2] + "+" + asbmlobject7[l7].getAttribute("specie") + "+" + asbmlobject7[l7].getAttribute("specie");
            }

            for(int k3 = 0; k3 < numspecies; k3++)
            {
                boolean flag8 = false;
                boolean flag12 = false;
                boolean flag10 = false;
                String s36 = "";
                String s38 = "";
                for(int k7 = 0; k7 < asbmlobject5.length; k7++)
                {
                    String s42 = asbmlobject5[k7].getAttribute("specie");
                    if(s42.equals(species_array[k3]))
                    {
                        if(asbmlobject5[k7].getAttribute("stoichiometry") != null)
                            s36 = "-" + asbmlobject5[k7].getAttribute("stoichiometry");
                        else
                            s36 = "-1";
                        flag8 = true;
                        flag10 = true;
                    }
                }

                for(int i8 = 0; i8 < i4; i8++)
                {
                    String s43 = asbmlobject7[i8].getAttribute("specie");
                    if(s43.equals(species_array[k3]))
                    {
                        if(asbmlobject7[i8].getAttribute("stoichiometry") != null)
                            s38 = asbmlobject7[i8].getAttribute("stoichiometry");
                        else
                            s38 = "1";
                        flag8 = true;
                        flag12 = true;
                    }
                }

                if(!flag8)
                    s7 = s7 + "0 ";
                else
                if(flag10)
                {
                    if(flag12)
                    {
                        int l9 = Integer.parseInt(s38) + Integer.parseInt(s36);
                        s7 = s7 + l9 + " ";
                        s11 = s11 + "    nu(" + k3 + "," + j2 + ") = " + l9 + ";\n";
                    } else
                    {
                        s7 = s7 + s36 + " ";
                        s11 = s11 + "    nu(" + k3 + "," + j2 + ") = " + s36 + ";\n";
                    }
                } else
                if(flag12)
                {
                    s7 = s7 + s38 + " ";
                    s11 = s11 + "    nu(" + k3 + "," + j2 + ") = " + s38 + ";\n";
                } else
                {
                    s7 = s7 + "x ";
                    s11 = s11 + "    nu(" + k3 + "," + j2 + ") = " + "error" + ";\n";
                }
            }

            s7 = s7 + "\n";
            s11 = s11 + "\n";
        }

        s11 = s11 + "    return nu;\n}\n\n";
        boolean flag1 = true;
        boolean flag2 = true;
        boolean flag4 = false;
        for(int k2 = 0; k2 < j1; k2++)
        {
            for(int l = 0; l < j1; l++)
            {
                int l4 = 1;
                boolean flag3 = true;
                for(boolean flag5 = false; !flag5 && ai[k2 + 1][l4] != 0; l4++)
                {
                    for(int j5 = 1; !flag5 && ai1[l + 1][j5] != 0;)
                        if(ai[k2 + 1][l4] == ai1[l + 1][j5])
                        {
                            ai2[k2][l] = 1;
                            flag5 = true;
                        } else
                        {
                            j5++;
                        }

                }

            }

        }

        for(int l2 = 0; l2 < j1; l2++)
        {
            int i5 = 0;
            for(int i1 = 0; i1 < j1; i1++)
                if(ai2[l2][i1] == 1)
                {
                    s12 = s12 + "     DG(" + l2 + "," + i5 + ") = " + i1 + "; \n";
                    i5++;
                }

            s12 = s12 + "     DG(" + l2 + "," + i5 + ") = " + -1 + "; \n\n";
        }

        s12 = s12 + "   return DG;\n}\n\n";
        System.out.println("Finished parsing reactions...generating output");
        s16 = s16 + ";\n\n";
        s17 = s17 + "    return a;\n}\n";
        s18 = s18 + "        } \n    }\n    return a;\n}\n";
        s20 = s20 + "\n    return j;\n}\n";
        s13 = s13 + s16 + s17 + "\n";
        s14 = s14 + s16 + s18 + "\n";
        s15 = s15 + s16 + s20 + "\n";
        s10 = s10 + s8 + s11 + s12 + s13 + s14 + s15 + s19;
        System.out.println("simulation time=" + s1);
        String s32 = "#include \"StochRxn.h\"\n";
        s32 = s32 + "#include \"ProblemDefinition.h\"\n";
        s32 = s32 + "#include \"SolverOptions.h\"\n";
        s32 = s32 + "#include \"CollectStats.h\"\n";
        s32 = s32 + "#include \"SSA.h\"\n";
        s32 = s32 + "#include \"Vector.h\"\n";
        s32 = s32 + "#include \"Matrix.h\"\n";
        s32 = s32 + "#include \"FixedStepsize.h\"\n";
        s32 = s32 + "#include \"ImplicitTau_SingleStep.h\"\n";
        s32 = s32 + "#include \"ExplicitTau_SingleStep.h\"\n";
        s32 = s32 + "#include \"Trapezoidal_SingleStep.h\"\n";
        s32 = s32 + "#include \"ImplicitTrapezoidal_SingleStep.h\"\n";
        s32 = s32 + "#include \"Midpoint_SingleStep.h\"\n";
        s32 = s32 + "#include \"Random.h\"\n";
        s32 = s32 + "#include <time.h>\n";
        s32 = s32 + "#include <iostream>\n";
        s32 = s32 + "#include <stdlib.h>\n\n";
        s32 = s32 + "using namespace CSE::Math;\n";
        s32 = s32 + "using namespace CSE::StochRxn;\n\n";
        s32 = s32 + "Vector Initialize();\n";
        s32 = s32 + "Matrix Stoichiometry();\n";
        s32 = s32 + "Matrix DependentGrapth();\n";
        s32 = s32 + "Vector Propensity(const Vector& x);\n";
        s32 = s32 + "Vector PartialProp(int RIndex, const Vector& x, const Matrix& dg,  double& a0);\n";
        s32 = s32 + "Matrix PropensityJacobian(const Vector& x);\n";
        s32 = s32 + "void Equilibrium(Vector& x, Vector& a, int rxn);\n\n";
        s32 = s32 + "int main(int argc, const char* argv[])\n{\n";
        s32 = s32 + "   try{\n";
        s32 = s32 + "      //parse arguments\n";
        s32 = s32 + "      int iterations;\n      const char* outFile;\n\n";
        s32 = s32 + "      if (argc != 3) {\n";
        s32 = s32 + "         std::cerr << \"Usage:  dimerstats <# runs> <output file>\";\n";
        s32 = s32 + "         exit(EXIT_FAILURE);\n";
        s32 = s32 + "      }\n";
        s32 = s32 + "      else {\n";
        s32 = s32 + "         iterations = atoi(argv[1]);\n";
        s32 = s32 + "         outFile = argv[2];\n";
        s32 = s32 + "      }\n\n";
        s32 = s32 + "      time_t curTime = time(0);\n";
        s32 = s32 + "      CSE::Math::Seeder(static_cast<unsigned int>(curTime), curTime);\n\n";
        s32 = s32 + "      // Set up the problem:\n";
        s32 = s32 + "      Vector X0 = Initialize();\n";
        s32 = s32 + "      Matrix nu = Stoichiometry();\n";
        s32 = s32 + "      Matrix dg = DependentGrapth();\n";
        s32 = s32 + "      ReactionSet rxns(nu, Propensity);\n\n";
        s32 = s32 + "      // Configure solver\n";
        s32 = s32 + "      SolverOptions opt = ConfigStochRxn(1,\"ossa\");\n";
        s32 = s32 + "      opt.stepsize_selector_func = SSADirect_Stepsize; \n";
        s32 = s32 + "      opt.single_step_func = SSA_SingleStep; \n";
        s32 = s32 + "      opt.progress_interval = 1000000;\n";
        s32 = s32 + "      opt.initial_stepsize = 0.001;\n";
        s32 = s32 + "      opt.absolute_tol = 1e-6;\n";
        s32 = s32 + "      opt.relative_tol = 1e-5;\n\n";
        s32 = s32 + "      double TimeFinal =" + s1 + ".0;\n\n";
        s32 = s32 + "      //Make the Run and report results\n";
        s32 = s32 + "      EndPtStats endpts = CollectStats(iterations, X0, 0, TimeFinal, rxns, opt);\n";
        s32 = s32 + "      WriteStatFile(endpts, outFile);\n";
        s32 = s32 + "      std::cerr << \"Done.\\n\";\n";
        s32 = s32 + "   }\n\n";
        s32 = s32 + "   catch (const std::exception& ex) {\n";
        s32 = s32 + "      std::cerr << \"\\nCaught \" << ex.what() << '\\n';\n";
        s32 = s32 + "   }\n\n";
        s32 = s32 + "   return 0;\n";
        s32 = s32 + "}\n";
        String s33 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
        s33 = s33 + "<sbml xmlns=\"http://www.sbml.org/sbml/level1\" level=\"1\" version=\"1\">" + '\n';
        s33 = s33 + "<model name=\"" + s5 + "\">" + '\n';
        s33 = s33 + "<notes> " + '\n';
        s33 = s33 + "<body xmlns=\"http://www.w3.org/1999/xhtml\">" + '\n';
        s33 = s33 + "<h1>" + s5 + "</h1>" + '\n';
        s33 = s33 + "<table border=\"0\" cellspacing=\"0\" cellpadding=\"2\" >" + '\n';
        s33 = s33 + "<thead>" + '\n';
        s33 = s33 + "<tr>" + '\n';
        s33 = s33 + "<th align=\"left\" valign=\"middle\" bgcolor=\"#eeeeee\">Reactions&#160;&#160;&#160;&#160;&#160;&#160;&#160;</th>" + '\n';
        s33 = s33 + "<th align=\"left\" valign=\"middle\" bgcolor=\"#eeeeee\">Rate</th>" + '\n';
        s33 = s33 + "</tr>" + '\n';
        s33 = s33 + "</thead>" + '\n';
        s33 = s33 + "<tbody>" + '\n';
        for(int i3 = 0; i3 < j1; i3++)
        {
            s33 = s33 + "<tr>" + '\n';
            s33 = s33 + "<td>" + as2[i3] + "&#160;" + " -->&#160;" + as3[i3] + "      </td>" + '\n';
            s33 = s33 + "<td>" + as1[i3] + "</td>" + '\n';
            s33 = s33 + "</tr>" + '\n';
        }

        s33 = s33 + "</tbody>" + '\n';
        s33 = s33 + "</table>" + '\n';
        s33 = s33 + "<h2>" + "inital value </h2>" + '\n';
        s33 = s33 + "<table border=\"0\" cellspacing=\"0\" cellpadding=\"2\" >" + '\n';
        s33 = s33 + "<thead>" + '\n';
        s33 = s33 + "<tr>" + '\n';
        s33 = s33 + "<th align=\"left\" valign=\"middle\" bgcolor=\"#eeeeee\">Reactions&#160;&#160;&#160;&#160;&#160;&#160;&#160;</th>" + '\n';
        s33 = s33 + "<th align=\"left\" valign=\"middle\" bgcolor=\"#eeeeee\">Rate</th>" + '\n';
        s33 = s33 + "</tr>" + '\n';
        s33 = s33 + "</thead>" + '\n';
        s33 = s33 + "<tbody>" + '\n';
        for(int j3 = 0; j3 < numspecies; j3++)
        {
            s33 = s33 + "<tr>" + '\n';
            s33 = s33 + "<td>" + "S" + (j3 + 1) + "  </td>" + '\n';
            s33 = s33 + "<td>" + as[j3] + "</td>" + '\n';
            s33 = s33 + "</tr>" + '\n';
        }

        s33 = s33 + "</tbody>" + '\n';
        s33 = s33 + "</table>" + '\n';
        s33 = s33 + "</body>" + '\n';
        s33 = s33 + "</notes>" + '\n';
        s33 = s33 + "</model>" + '\n';
        s33 = s33 + "</sbml>" + '\n';
        System.out.println("Writing files, please wait...");
        try
        {
            File file = new File(s5);
            if(!file.isDirectory())
                file.mkdir();
        }
        catch(Exception _ex)
        {
            System.out.println("Unable to make dir!");
        }
        try
        {
            BufferedWriter bufferedwriter = new BufferedWriter(new FileWriter(s5 + "/ProblemDefinition.cpp"));
            bufferedwriter.write(s10);
            bufferedwriter.close();
        }
        catch(IOException _ex)
        {
            JOptionPane.showMessageDialog(null, "Unable to save to prop.c", null, 1);
            System.exit(0);
        }
        try
        {
            BufferedWriter bufferedwriter1 = new BufferedWriter(new FileWriter(s5 + "/Stat.cpp"));
            bufferedwriter1.write(s32);
            bufferedwriter1.close();
        }
        catch(IOException _ex)
        {
            System.exit(0);
        }
        try
        {
            BufferedWriter bufferedwriter2 = new BufferedWriter(new FileWriter(s5 + "/forview" + s));
            bufferedwriter2.write(s33);
            bufferedwriter2.close();
        }
        catch(IOException _ex)
        {
            System.exit(0);
        }
    }

    public boolean requiresValidXML()
    {
        return false;
    }

    public static void main(String args[])
    {
        String s = "";
        String s1 = "";
        if(args.length != 2)
        {
            System.exit(0);
        } else
        {
            s = args[0];
            s1 = args[1];
        }
        new Converter(s, s1);
    }

    String species_array[];
    int numspecies;
}