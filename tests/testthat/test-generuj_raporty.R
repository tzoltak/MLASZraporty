context("Generowanie raportów")

wskaznikiSzk$SZK_kod = unlist(wskaznikiSzk$SZK_kod)
stanFolderu = list.files()

# test_that("Raporty PDF na podstawie szablonu 'raport_szkoly.RMD'", {
#   nazwyRaportowPDF =
#     generuj_raporty('raport_szkoly.Rmd',
#                     wskaznikiSzk,
#                     wskaznikiTypSzk,
#                     SZK_kod,
#                     parametry = list(typDokumentu = "pdf",
#                                      progLiczebnosci = 10,
#                                      rocznik = 2017,
#                                      wyrownanieTabWykr = "center"))
#   expect_equal(paste0(wskaznikiSzk$SZK_kod, ".pdf"),
#                nazwyRaportowPDF)
# })
#
test_that("Raporty HTML na podstawie szablonu 'raport_szkoly.RMD'", {
  nazwyRaportowPDF =
    generuj_raporty('raport_szkoly.Rmd',
                    wskaznikiSzk,
                    wskaznikiTypSzk,
                    SZK_kod,
                    parametry = list(typDokumentu = "html",
                                     progLiczebnosci = 10,
                                     rocznik = 2017,
                                     wyrownanieTabWykr = "center"))
  expect_equal(paste0(wskaznikiSzk$SZK_kod, ".html"),
               nazwyRaportowPDF)
})

unlink(setdiff(list.files(), stanFolderu), recursive = TRUE)
