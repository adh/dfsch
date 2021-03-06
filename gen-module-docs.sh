#!/bin/sh

mkdir documentation/modules/ 2>/dev/null

cat > documentation/modules/index.html <<EOF 
<!DOCTYPE html><html xmlns="http://www.w3.org/1999/xhtml">
<head><title>dfsch standard modules documentation</title>
EOF

if [ -f "$DOCGEN_HEAD_FILE" ]; then
    cat "$DOCGEN_HEAD_FILE" >> documentation/modules/index.html
fi

cat >> documentation/modules/index.html <<EOF 
</head>
<body>
<h1>dfsch standard modules documentation</h1>
<ul>
EOF

for i in $2; do
    cmdpart='';
    if [ -x $1/doc/module/${i}.md ]; then
        cmdpart="--chapters $1/doc/module/$i.md";
    fi;
    ./dfsch-run -L ./.libs -L $1/lib-scm \
        $1/tools/docgen.scm --module $i --package-exported $i \
        documentation/modules/${i} #>/dev/null 2>/dev/null

    if [ $? -eq 0 ]; then
        echo ${i}... OK
        echo "<li><a href='${i}/'>${i}</a></li>" \
            >> documentation/modules/index.html
    else
        echo ${i}... Error
    fi  
done

cat >> documentation/modules/index.html <<EOF
</ul>
</body>
</html>
EOF