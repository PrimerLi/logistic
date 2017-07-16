#include <iostream>
#include <vector>
#include <string>
#include <cmath>
#include <fstream>
#include <cassert>
#include <cstdlib>
#include <cmath>
using namespace std;

class Vector
{
private:
    int length;
    vector<double> array;
public:
    Vector() {}
    explicit Vector(int len)
    {
        this->length = len;
        for (int i = 0; i < len; ++i)
        {
            array.push_back(0);
        }
    }
    Vector(const Vector &parameter)
    {
        this->length = parameter.length;
        for (int i = 0; i < length; ++i)
        {
            array.push_back(parameter.array[i]);
        }
    }
    const Vector & operator= (const Vector &parameter)
    {
        if (&parameter == this)
        {
            return *this;
        }
        else
        {
            array.clear();
            this->length = parameter.length;
            for (int i = 0; i < length; ++i)
            {
                array.push_back(parameter.array[i]);
            }
            return *this;
        }
    }
    Vector operator+ (const Vector &parameter)
    {
        Vector result(length);
        result.length = parameter.length;
        assert(this->length == parameter.length);
        for (int i = 0; i < length; ++i)
        {
            result.array[i] = array[i] + parameter.array[i];
        }
        return result;
    }
    
    Vector operator- (const Vector &parameter)
    {
        Vector result(length);
        result.length = parameter.length;
        assert(length == parameter.length);
        for (int i = 0; i < length; ++i)
        {
            result.array[i] = array[i] - parameter.array[i];
        }
        return result;
    }
    Vector operator* (double scale)
    {
        Vector result(length);
        result.length = length;
        for (int i = 0; i < length; ++i)
        {
            result.array[i] = array[i]*scale;
        }
        return result;
    }
    double operator[] (int index) const
    {
        assert(index < length && index >= 0);
        return array[index];
    }
    double & operator[] (int index)
    {
        assert(index < length && index >= 0);
        return array[index];
    }
    void print() const
    {
        for (int i = 0; i < length; ++i)
        {
            cout << array[i] << ", ";
        }
        cout << endl;
    }
    double normSquared() const
    {
        double s = 0;
        for (int i = 0; i < array.size(); ++i)
        {
            s = s + array[i]*array[i];
        }
        return s;
    }
    double norm() const
    {
        return sqrt(normSquared());
    }
    friend Vector operator* (double scale, const Vector &parameter)
    {
        Vector result(parameter.length);
        result.length = parameter.length;
        for (int i = 0; i < parameter.length; ++i)
        {
            result.array[i] = scale*parameter.array[i];
        }
        return result;
    }
    friend ostream & operator<< (ostream &os, const Vector &parameter)
    {
        for (int i = 0; i < parameter.length; ++i)
        {
            os << parameter.array[i] << ", ";
        }
        return os;
    }
};

class Logistic
{
private:
    vector<double> hours, pass;
public:
    Logistic() {}
    Logistic(string fileName)
    {
        ifstream ifile(fileName.c_str());
        if (!ifile.is_open())
        {
            cout << fileName << " does not exist. " << endl;
            exit(-1);
        }
        double h, p;
        while(!ifile.eof())
        {
            ifile >> h >> p;
            hours.push_back(h);
            pass.push_back(p);
        }
        ifile.close();
    }
    vector<double> getHours() const
    {
        return hours;
    }
    vector<double> getPass() const
    {
        return pass;
    }
    void print() const
    {
        assert(hours.size() == pass.size());
        for (int i = 0; i < hours.size(); ++i)
        {
            cout << hours[i] << "  " << pass[i] << endl;
        }
    }
    double mll(double alpha, double beta) const
    {
        double s = 0;
        for (int i = 0; i < hours.size(); ++i)
        {
            s = s + pass[i]*log(1 + exp(-alpha - beta*hours[i])) + (1 - pass[i])*log(1 + exp(alpha + beta*hours[i]));
        }
        return s;
    }
    Vector gradient(double alpha, double beta) const
    {
        int len = 2;
        Vector result(2);
        double s = 0;
        for (int i = 0; i < hours.size(); ++i)
        {
            s = s + pass[i]*(-1/(1 + exp(alpha + beta*hours[i]))) + (1 - pass[i])*(1/(1 + exp(-alpha - beta*hours[i])));
        }
        result[0] = s;
        s = 0;
        for (int i = 0; i < hours.size(); ++i)
        {
            s = s + (-hours[i]*pass[i])*(1/(1 + exp(alpha+beta*hours[i]))) + (hours[i]*(1 - pass[i]))*(1/(exp(-alpha - beta*hours[i]) + 1));
        }
        result[1] = s;
        return result;
    }
    Vector gradientDescent(double alpha, double beta) const
    {
        Vector result(2);
        result[0] = alpha;
        result[1] = beta;
        double eps = 1e-6;
        double diff = 0;
        int iterationMax = 1200;
        int count = 0;
        double rate = 0.01;
        while(true)
        {
            Vector temp(result);
            result = result - rate*gradient(result[0], result[1]);
            diff = (result - temp).norm();
            if (diff < eps) break;
            count++;
            if (count > iterationMax) break;
        }
        return result;
    }
    double logisticFunction(double x, double alpha, double beta) const
    {
        return 1/(1 + exp(-alpha - beta*x));
    }
    void logisticRegression(double alpha, double beta) const
    {
        vector<double> x;
        vector<double> probabilty;
        int size = 30;
        double lower = 0;
        double upper = 6;
        double dx = (upper - lower)/(size);
        for (int i = 0; i <= size; ++i)
        {
            x.push_back(lower + dx*i);
        }
        Vector result = gradientDescent(alpha, beta);
        cout << result << endl;
        cout << gradient(result[0], result[1]) << endl;
        for (int i = 0; i <= size; ++i)
        {
            probabilty.push_back(logisticFunction(x[i], result[0], result[1]));
        }
        ofstream ofile;
        ofile.open("regression.txt");
        for (int i = 0; i < x.size(); ++i)
        {
            ofile << x[i] << "  " << probabilty[i] << endl;
        }
        ofile.close();
    }
};

int main()
{
    Logistic analysis("data.txt");
    double alpha, beta;
    alpha = -4;
    beta = 1.48;
    analysis.logisticRegression(alpha, beta);
    return 0;
}
