import {
  Component,
  computed,
  inject,
  input,
  model,
  signal,
} from '@angular/core';
import {
  defaultDokument,
  Sadrzaj,
  Sekcija,
  TokenService,
  UserService,
} from '../../core';
import { ContentCardComponent } from '../content-card/content-card.component';
import { openModal } from '../modal/modal.component';
import { Dialog, DialogRef } from '@angular/cdk/dialog';
import { SectionEditComponent } from '../section-edit/section-edit.component';
import { ContentAddComponent } from '../content-add/content-add.component';
import {
  dodaj_dokument,
  dodaj_lekciju,
  dodaj_poveznicu,
  dodaj_sadrzaj_na_sekciju,
  dohvati_sekciju,
  uredi_sekciju,
} from '../../../assets/pkg/client';

@Component({
  selector: 'app-section',
  standalone: true,
  imports: [ContentCardComponent],
  templateUrl: './section.component.html',
  styleUrl: './section.component.scss',
})
export class SectionComponent {
  public sekcija = model.required<Sekcija>();
  public isWorker = input.required<boolean>();
  public user = inject(UserService);
  private token = inject(TokenService);
  private dialog = inject(Dialog);
  public sadrzaj = computed(() => {
    return this.sekcija()?.sadrzaj;
  });

  public open = signal(true);

  openEdit(): void {
    let ref = openModal<Sekcija>(this.dialog, {
      data: structuredClone(this.sekcija()),
      tool: { view: SectionEditComponent },
      inputs: {},
      title: 'Edit section',
    });
    ref.componentInstance['query'].subscribe((section: Sekcija) => {
      if (section) {
        uredi_sekciju(
          section.id,
          section.naziv,
          section.opis,
          section.vidljivo,
          this.token.accessToken(),
        ).then((res) => {
          dohvati_sekciju(res, this.token.accessToken()).then((sekcija) => {
            this.sekcija.set(sekcija);
          });
        });
      }
      ref.close();
    });
  }

  openEditContent(): void {
    let ref = openModal<Sadrzaj>(this.dialog, {
      data: structuredClone(defaultDokument()),
      tool: { view: ContentAddComponent },
      inputs: {},
      title: 'Add content',
    });
    ref.componentInstance['query'].subscribe((content: Sadrzaj) => {
      if (content) {
        console.log(content);
        if (content.tip === 'dokument') {
          dodaj_dokument(
            content.naziv,
            content.vrijednost.referenca,
            this.token.accessToken(),
          ).then((res) => {
            dodaj_sadrzaj_na_sekciju(
              this.sekcija().id,
              res,
              this.token.accessToken(),
            ).then((res) => {
              if (res) {
                console.log(res);
                dohvati_sekciju(
                  this.sekcija().id,
                  this.token.accessToken(),
                ).then((sekcija) => {
                  this.sekcija.set(sekcija);
                });
              }
            });
          });
        } else if (content.tip === 'poveznica') {
          dodaj_poveznicu(
            content.naziv,
            content.vrijednost.referenca,
            this.token.accessToken(),
          ).then((res) => {
            dodaj_sadrzaj_na_sekciju(
              this.sekcija().id,
              res,
              this.token.accessToken(),
            ).then((res) => {
              if (res) {
                dohvati_sekciju(
                  this.sekcija().id,
                  this.token.accessToken(),
                ).then((sekcija) => {
                  this.sekcija.set(sekcija);
                });
              }
            });
          });
        } else {
          dodaj_lekciju(
            content.naziv,
            content.vrijednost.sadrzaj,
            this.token.accessToken(),
          ).then((res) => {
            dodaj_sadrzaj_na_sekciju(
              this.sekcija().id,
              res,
              this.token.accessToken(),
            ).then((res) => {
              if (res) {
                dohvati_sekciju(
                  this.sekcija().id,
                  this.token.accessToken(),
                ).then((sekcija) => {
                  this.sekcija.set(sekcija);
                });
              }
            });
          });
        }
      }
      ref.close();
    });
  }
}
