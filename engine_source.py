import os,sys,json,urllib.request,urllib.error
U=os.environ.get('MAKEFUN_ENV_URL') or sys.exit('MAKEFUN_ENV_URL is not set')
V={'p':100,'n':320,'b':330,'r':500,'q':900,'k':20000}
D={'p':[(1,0),(2,0),(1,1),(1,-1)],'n':[(2,1),(2,-1),(-2,1),(-2,-1),(1,2),(1,-2),(-1,2),(-1,-2)],'b':[(1,1),(1,-1),(-1,1),(-1,-1)],'r':[(1,0),(-1,0),(0,1),(0,-1)],'q':[(1,1),(1,-1),(-1,1),(-1,-1),(1,0),(-1,0),(0,1),(0,-1)],'k':[(1,1),(1,-1),(-1,1),(-1,-1),(1,0),(-1,0),(0,1),(0,-1)]}
KN=[33,31,18,14,-14,-18,-31,-33]; KI=[16,17,15,1,-1,-15,-17,-16]; BI=[15,17,-15,-17]; RI=[16,-16,1,-1]; QI=BI+RI
NODES=18000; TT={}; hist={}; killers=[None]*64
def post(p,b):
 r=urllib.request.Request(U+p,data=json.dumps(b).encode(),headers={'content-type':'application/json'})
 try:
  with urllib.request.urlopen(r,timeout=15) as x:return json.load(x)
 except Exception as e: sys.exit(str(e))
def sq(s): return (ord(s[0])-97)+(int(s[1])-1)*16
def name(x): return chr(97+(x&7))+str((x>>4)+1)
def parse(f):
 a=[None]*128; z=f.split(); rows=z[0].split('/')
 for rr,row in enumerate(rows):
  x=0
  for c in row:
   if c.isdigit():x+=int(c)
   else:a[(7-rr)*16+x]=c;x+=1
 return a,z[1],z[2],z[3]
def attacked(a,x,by):
 for d in KN:
  y=x+d
  if not y&136 and a[y]==('N' if by else 'n'):return 1
 for d in BI:
  y=x+d
  while not y&136:
   c=a[y]
   if c:
    if c in ('B','Q') if by else c in ('b','q'):return 1
    break
   y+=d
 for d in RI:
  y=x+d
  while not y&136:
   c=a[y]
   if c:
    if c in ('R','Q') if by else c in ('r','q'):return 1
    break
   y+=d
 for d in KI:
  y=x+d
  if not y&136 and a[y]==('K' if by else 'k'):return 1
 for d in (15,17):
  y=x+(d if by else -d)
  if not y&136 and a[y]==('P' if by else 'p'):return 1
 return 0
def check(a,side):
 k='K' if side else 'k';x=a.index(k);return attacked(a,x,not side)
def gen(a,side,cast,ep):
 out=[]
 for x,c in enumerate(a):
  if not c or c.isupper()!=side:continue
  p=c.lower()
  if p=='p':
   dr=16 if side else -16; y=x+dr
   if not y&136 and not a[y]:
    if y>>4 in (0,7):out += [(x,y,q) for q in 'qrbn']
    else:
     out.append((x,y,''));
     if x>>4==(1 if side else 6) and not a[x+2*dr]:out.append((x,x+2*dr,''))
   for d in (15,17):
    y=x+(d if side else -d)
    if y&136:continue
    if a[y] and a[y].isupper()!=side or y==ep:
     out += [(x,y,q) for q in ('qrbn' if y>>4 in (0,7) else ('',))]
  else:
   ds=KN if p=='n' else KI if p=='k' else BI if p=='b' else RI if p=='r' else QI
   for d in ds:
    y=x+d
    while not y&136:
     if not a[y]:out.append((x,y,''))
     else:
      if a[y].isupper()!=side:out.append((x,y,''))
      break
     if p in 'nk':break
     y+=d
   if p=='k':
    rank=0 if side else 7
    if x==rank*16+4 and not check(a,side):
     for to,rook,clear in ((rank*16+6,rank*16+7, (5,6)),(rank*16+2,rank*16+0,(1,2,3))):
      if cast[(0 if side else 2)+(to<x)] and a[rook] and all(not a[rank*16+i] for i in clear) and all(not attacked(a,rank*16+i,not side) for i in ((5,6) if to>x else (3,2))):out.append((x,to,''))
 legal=[]
 for m in out:
  b=a[:];x,y,p=m;c=b[x];b[x]=None;b[y]=c
  if c.lower()=='p' and y==ep and not a[y]:b[y+(-16 if side else 16)]=None
  if c.lower()=='k' and abs(y-x)==2:
   r=y&112;rx=r+7 if y>x else r;rt=r+5 if y>x else r+3;b[rt]=b[rx];b[rx]=None
  if p:b[y]=p.upper() if side else p
  if not check(b,side):legal.append(m)
 return legal
def apply(a,m,side,ep):
 x,y,p=m;b=a[:];c=b[x];b[x]=None;b[y]=c
 if c.lower()=='p' and y==ep and not a[y]:b[y+(-16 if side else 16)]=None
 if c.lower()=='k' and abs(y-x)==2:
  r=y&112;rx=r+7 if y>x else r;rt=r+5 if y>x else r+3;b[rt]=b[rx];b[rx]=None
 if p:b[y]=p.upper() if side else p
 return b
def val(a):return sum((V.get(c.lower(),0) if c.isupper() else -V.get(c.lower(),0)) for c in a if c)
def search(a,side,cast,ep,d,al,be):
 global NODES
 NODES-=1
 ms=gen(a,side,cast,ep)
 if not ms:return (-30000-d if check(a,side) else 0,None)
 if d<=0 or NODES<=0:return ((val(a) if side else -val(a)),None)
 best=None
 for m in sorted(ms,key=lambda m: (0 if a[m[1]] else 1,-V.get((m[3] if len(m)>3 else '') ,0))):
  ne=(m[0]+m[1])//2 if a[m[0]].lower()=='p' and abs(m[1]-m[0])==32 else -1
  cc=cast[:];x,y=m[:2];pc=a[x].lower()
  if pc=='k':cc[0 if side else 2]=cc[1 if side else 3]=0
  for j,z in enumerate((7,0,119,112)):
   if x==z or y==z:cc[j]=0
  s,_=search(apply(a,m,side,ep),not side,cc,ne,d-1,-be,-al)
  s=-s
  if s>al:al=s;best=m
  if al>=be:break
 return al,best
def choose(f):
 global NODES
 a,turn,ca,eps=parse(f);side=turn=='w';cast=[ca[0] in ca,ca[1] in ca,ca[2] in ca,ca[3] in ca];ep=sq(eps) if eps!='-' else -1;best=None;d=1
 while d<5:
  NODES=18000;s,m=search(a,side,cast,ep,d,-40000,40000)
  if m:best=m
  d+=1
 if not best:best=gen(a,side,cast,ep)[0]
 return name(best[0])+name(best[1])+(best[2] if best[2] else '')
while 1:
 g=post('/game',{})
 if g.get('status')=='done':break
 while g.get('status')=='your_move':
  m=choose(g['fen']);g=post('/move',{'game':g['game'],'move':m})
