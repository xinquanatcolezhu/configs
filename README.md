# configs
Configs on Unix at work

Testing mermaid

```mermaid
graph LR
   diamond{diamond}-->square
   square-->square2
   square2-->square3
   square3-->square4
   square2-.->square4
```


```mermaid

graph TD

start(Does Gitlab support mermaid flowcharts?)

choice{Is this displayed as a flowchart?}

does(Awesome!)

doesnt(maybe there is a syntax error somewhere?)

start-->choice

choice--Yes-->does

choice--No-->doesnt

```
