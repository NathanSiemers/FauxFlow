git commit -a -m update
#git push origin master
git push github master
rsync -av *.R template.docx *.Rmd restart.txt lib ../SCHCC/
rsync -av *.R template.docx *.Rmd restart.txt lib ../SCBlood/
rsync -av *.R template.docx *.Rmd restart.txt lib ../SCMel/
rsync -av *.R template.docx *.Rmd restart.txt lib ec2-user@shiny.rwc.bms.com:Shiny/SCHCC/
rsync -av *.R template.docx *.Rmd restart.txt lib ec2-user@shiny.rwc.bms.com:Shiny/SCBlood/
rsync -av *.R template.docx *.Rmd restart.txt lib ec2-user@shiny.rwc.bms.com:Shiny/SCMel/
##rsync -av *.R template.docx *.Rmd restart.txt lib nathan@shiny.fiveprime.org:Shiny/SCHCC/
##rsync -av *.R template.docx *.Rmd restart.txt lib nathan@shiny.fiveprime.org:Shiny/SCBlood/
##rsync -av *.R template.docx *.Rmd restart.txt lib nathan@shiny.fiveprime.org:Shiny/SCMel/


