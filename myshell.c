// Building a shell program from scratch to learn about shell 
// functionalities, process interaction, and defensive programming.
// It is capable of parsing a command line of 512 characters or less 
// with ; | >.

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/wait.h>

int stdi = STDIN_FILENO;
int stdo = STDOUT_FILENO;
char error_message[30] = "An error has occurred\n";
char directory[512]= "myshell> ";

void myPrint(char *msg)
{
  write(stdo, msg, strlen(msg));
}


typedef struct redirection_token redirection_token;
struct redirection_token{
  char* val;
  redirection_token *next;
}; // linked list of strings separated by empty space  

typedef struct cmd cmd;
struct cmd{
  redirection_token* rdt;
  cmd *next;
}; //separates commands around redirection character '>'      

typedef struct cmd_line cmd_line;
struct cmd_line{
  cmd* cmd;
  cmd_line *next;
}; //stores individual commands (separated by ';') in a linked list

redirection_token* rt_singleton(char* s) {
  redirection_token *rt=malloc(sizeof(redirection_token));
  rt->val = s;
  rt->next = NULL;
  return rt;
}

redirection_token* parseRedToken(char* input) {
  redirection_token *top, *rt;
  char *rt_s,*saveptr;

  rt_s = strtok_r(input, " \t", &saveptr);
  //myPrint(rt_s);
  char* rt_s2 = strdup(rt_s);
  rt = rt_singleton(rt_s2);
  top = rt;

  while(rt_s) {
    rt_s = strtok_r(NULL," \t",&saveptr);
    if (rt_s==NULL)
      break;
    rt_s2=strdup(rt_s);
    rt->next = rt_singleton(rt_s2);
    rt = rt->next;
  }
  return top;
}

int numRedTok(redirection_token* rt){
  int rv=0;
  while (rt){
    rv++;
    rt = rt->next;
  }
  return rv;
}

int redirect(cmd* cmd){
  int rv=0;
  while (cmd!=NULL){
    rv++;
    cmd = cmd->next;
  }
  return rv;
}

cmd* cmd_singleton(redirection_token* rd) {
  cmd *cm = malloc(sizeof(cmd));
  cm->rdt = rd;
  cm->next = NULL;
  return cm;
}

cmd* parseCmd(char* input) {
  cmd *top, *cd;
  char *cd_s,*saveptr;

  cd_s = strtok_r(input, ">",&saveptr);
  cd = cmd_singleton(parseRedToken(cd_s));
  top = cd;
  //myPrint(cd_s);
  while(cd_s) {
    cd_s = strtok_r(NULL,">",&saveptr);
    //myPrint(cd_s);
    if (cd_s==NULL)
      break;
    cd->next = cmd_singleton(parseRedToken(cd_s));
    cd = cd->next;
  }
  // myPrint("1 ");
  // myPrint(cd->rdt->val);
  // myPrint("2 ");
  // myPrint(top->rdt->val);
  // myPrint("3 ");
  // myPrint(top->next->rdt->val);
  return top;
}

cmd_line* cmdline_singleton(cmd* cmd) {
  cmd_line* cl=malloc(sizeof(cmd_line));
  cl->cmd = cmd;
  cl->next = NULL;
  return cl;
}

cmd_line* parseCmdLine(char* input) {
  cmd_line *top, *cl;
  char *cl_s,*saveptr;

  cl_s = strtok_r(input,";",&saveptr);
  cl = cmdline_singleton(parseCmd(cl_s));
  top = cl;
  while(cl_s) {
    cl_s = strtok_r(NULL,";",&saveptr);
    if (cl_s==NULL)
      break;
    cl->next = cmdline_singleton(parseCmd(cl_s));
    cl = cl->next;
  }
  return top;
}

char **set_args(redirection_token* rdt){
  //myPrint("where we at");
  int len = numRedTok(rdt);
  char **rv=malloc(sizeof(char*)*len);
  char **top;
  top = rv;
  while(rdt){
    //myPrint(rdt->val);
    //*rv=strndup(rdt->val,strlen(rdt->val)-1);
    *rv = rdt->val;
    myPrint(rdt->val);
    //myPrint("did we crash");
    //myPrint(*rv);
    //myPrint("did we crash2");
    rv++;
    //if (rdt->next == NULL)
    //  myPrint("let me know");
    rdt=rdt->next;
  }
  //myPrint("we outtie");
  //myPrint(*top);
  //sleep(1);
  myPrint("top");
  myPrint(top[0]);
  myPrint("top2");
  myPrint(top[1]);
  return top;
}

void myPWD(char *rv) {
  getcwd(rv,512);
  return;
  // the caller has to be responsible for printing this out
}

void myCD(char* path) {
  // idea # 2
  // check if it matches PWD
  // if it does not, treat as full path
  // chdir into that full path
  // if full path, 

  // chdir() on error -1 is returned
  // there will be a potential problem if chdir() does not merely return a -1
  // and returns an error message

  // we need to check if there is an argument before calling
  // if there isnt an argument return getenv(HOME)
  path = strndup(path,strlen(path));
  if (path == NULL)
    getenv("HOME");
  if (chdir(path) == -1) { // check if it isnt a full path
    char PWD[512];
    myPWD(PWD);
    strcat(PWD, "/");
    //myPrint(path);
    strcat(PWD, path);
    //myPrint(PWD);
    if (chdir(PWD) == -1) // check folder - path not found
      write(stdo, error_message, strlen(error_message));
  }
}

void freeRedToken(redirection_token* rt) {
  free(rt->val);
  if (rt->next)
    freeRedToken(rt->next);
  free(rt);
}

void freeCmd(cmd* cmd) {
  freeRedToken(cmd->rdt);
  if (cmd->next)
    free(cmd->next);
  free(cmd);
}

void freeCmdLine(cmd_line* cl) {
  freeCmd(cl->cmd);
  if (cl->next)
    free(cl->next);
  free(cl);
}

void exe(cmd* cmd){
  // myPrint(cmd->rdt->val);
  // myPrint(cmd->rdt->next->val);
  int f;
  char *p,*c,*e;
  // char buf[4];
  // sprintf(buf,"%d",redirect(cmd));
  // myPrint(buf);
  //    char * hello= (char*)malloc(sizeof(cmd->rdt->val));
  //    sprintf(buf, "%d", (int)strlen(cmd->rdt->val));
  //myPrint(buf);
  char *lo=malloc(sizeof(char)*(strlen(cmd->rdt->val)+1));
  lo=cmd->rdt->val;
  //myPrint("lo");
  //myPrint(lo);
  p=strdup("pwd");
  c=strdup("cd");
  e=strdup("exit");
  if (redirect(cmd)==2){
    //myPrint("redirect");
    
    if(numRedTok(cmd->rdt) != 1){
        char duf[4];
        sprintf(duf,"%d",numRedTok(cmd->rdt->next));
        myPrint(duf);
      //myPrint("branch 1");
      write(stdo, error_message, strlen(error_message));
      return;
    }
    //myPrint("open");
    //int f = open(cmd->next->rdt->val, O_RDWR | O_CREAT, S_IWUSR | S_IWGRP | S_IWOTH);
    
    //myPrint("open1.2");
    // myPrint(cmd->rdt->val);
    // myPrint(cmd->rdt->next->val);
    if ((f = open(cmd->next->rdt->val, O_RDWR | O_APPEND | S_IRUSR)) == -1) {
      //myPrint("open1.5");
      f = creat(cmd->next->rdt->val, O_RDWR | S_IRUSR);
    }
    //myPrint("open2");
    // char euf[4];
    // sprintf(euf,"%d",f);
    // myPrint(euf);
    stdo = f;
    //dup2(f,1);
    // close(1);
    // dup(f);
    //myPrint("open3");
  } else if (redirect(cmd) >2){
    //myPrint("branch 2");
    write(stdo, error_message, strlen(error_message));
    return;
  }
  if(!strncmp(lo, p,3 )){
    char pwd[512];
    myPWD(pwd);
    write(stdo, pwd, strlen(pwd));
    return;
  }
  if(!strncmp(lo,c,2)){
    char* path=cmd->rdt->next->val;
    myCD(path);
    return;
  }
  if(!strncmp(lo,e,4))
    exit(0);
  pid_t pid;
  int status;
  char **args=set_args(cmd->rdt);
  //myPrint(args[0]);
  //myPrint(args[1]);
  //myPrint(args[2]);
  //myPrint(args[3]);
  myPrint("   b4 fork  ");
  myPrint(*args);
  myPrint(args[0]);
  myPrint(args[1]);
  myPrint(args[2]);
  if (!(pid = fork())){ // 0 for child process
    if (execvp(*args,args)<0){
      myPrint("   after fork  ");
      write(stdo, error_message, strlen(error_message));
    }
    // stdo = 1;
    // dup2(f, 1);
    // close(f);
  } else
    while (wait(&status) != pid);
  stdo = 1;
  dup2(f, 1);
  close(f);
  // close(stdo);
  // dup(stdo);
}

int main(int argc, char *argv[])
{

  char cmd_buff[514]; //shouldn't this be 514?
  char *pinput;

  // batch mode
  if (argc == 2) {

    int batch = open(argv[1], O_RDONLY); // open file to read only
    // if fscanf > 514, we print but not use
    stdi=batch;
    while (read(stdi,cmd_buff,514)){
      myPrint(cmd_buff);
      cmd_line* cl=parseCmdLine(cmd_buff);
      while(cl->cmd){
  exe(cl->cmd);
  cl=cl->next;
      }
      freeCmdLine(cl);
    }
    // after we reach the end of file
    exit(1);
  }
  while (1) {
    if (stdo != 1)
      close(stdo);
    //dup(1);
    stdo = 1;
    //dup(1);
    // char euf[4];
    // sprintf(euf,"%d",stdo);
    // myPrint(euf);
    // dup(1);
    // myPrint(stdo);
    // stdo = 1;
    // dup2(1,1);
    myPrint("myshell> ");
    pinput = fgets(cmd_buff, 514, stdin);
    pinput = strndup(pinput, strlen(pinput)-1);
    cmd_line* cls=parseCmdLine(pinput);
    // printf("%s\n", cls->cmd->rdt->val);
    // printf("%s\n", cls->cmd->rdt->next->val);
    while(cls->cmd){
      exe(cls->cmd);
      //      myPrint("that");
      if(cls->next)
  cls=cls->next;
      else{
  myPrint("\n");
  cls->cmd = NULL;
      }    
    }

       //freeCmdLine(cls);
    if (!pinput) {
      exit(0);
    }
  }
}
