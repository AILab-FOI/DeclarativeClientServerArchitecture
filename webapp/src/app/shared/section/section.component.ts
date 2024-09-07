import {
  Component,
  computed,
  inject,
  input,
  model,
  output,
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
import { Dialog } from '@angular/cdk/dialog';
import { SectionEditComponent } from '../section-edit/section-edit.component';
import { ContentAddComponent } from '../content-add/content-add.component';
import {
  dodaj_dokument,
  dodaj_lekciju,
  dodaj_poveznicu,
  dodaj_sadrzaj_na_sekciju,
  obrisi_sadrzaj,
  obrisi_sekciju,
  uredi_sekciju,
} from '../../../assets/pkg/client';
import { ButtonComponent } from '../button/button.component';

@Component({
  selector: 'app-section',
  standalone: true,
  imports: [ContentCardComponent, ButtonComponent],
  templateUrl: './section.component.html',
  styleUrl: './section.component.scss',
})
export class SectionComponent {
  public sekcija = model.required<Sekcija>();
  public isWorker = input.required<boolean>();
  public user = inject(UserService);
  public query = output<boolean>();
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
          this.query.emit(true);
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
                this.query.emit(true);
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
                this.query.emit(true);
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
                this.query.emit(true);
              }
            });
          });
        }
      } else {
        this.query.emit(false);
      }
      ref.close();
    });
  }

  delete(pom: boolean, id: number): void {
    if (pom) {
      obrisi_sadrzaj(id, this.token.accessToken()).then((_) =>
        this.query.emit(true),
      );
    }
  }

  deleteSection(res: boolean): void {
    if (res) {
      obrisi_sekciju(this.sekcija().id, this.token.accessToken()).then((_) => {
        this.query.emit(true);
      });
    }
  }
}
