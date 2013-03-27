#include<iostream>
#include "visitor.h"

using namespace std;
class Node;
class Red;
class Blue;

class Visitor{
    public:
        virtual void visit(Red*) = 0;
        virtual void visit(Blue*) = 0;
};

class Node {
    public:
        virtual void accept(Visitor&) = 0;
        virtual void print() = 0;
};

class Red :public Node {
    public:
        void accept(Visitor& vis){
            vis.visit(this);
        }
        void print(){
            cout << "R," ;
        }
};

class Blue : public Node{
    public:
        void accept(Visitor& vis){
            vis.visit(this);
        }
        void print(){
            cout << "B," ;
        }
};

class CountVisitor: public Visitor{
    int redCount;
    int blueCount;
    public:
    CountVisitor(){
        redCount = 0;
        blueCount = 0;
    }
    void visit(Red* node){
        redCount++;
    }
    void visit(Blue* node){
        blueCount++;
    }
    void print(){
        cout << "Red = " << redCount << ", Blue = " << blueCount << endl;
    }
};

class PrintVisitor: public Visitor{
    public:
        void visit(Red* node){
            node->print();
        }
        void visit(Blue* node){
            node->print();
        }
};

int execute(int val){

    Node* list[] = {new Red, new Blue, new Red, new Red, new Blue, 0};

    CountVisitor cntVis;
    PrintVisitor prntVis;

    for (int i = 0; *(list+i) != 0; ++i){
        (*(list+i))->accept(cntVis);
        (*(list+i))->accept(prntVis);
    }
    cntVis.print();
    cout << "Inp val was " << val << endl;
    return val;
}

